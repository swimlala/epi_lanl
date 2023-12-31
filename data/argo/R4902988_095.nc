CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T22:56:17Z creation;2022-06-04T22:56:18Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604225617  20220609221504  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               _A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���ۗS1   @����o��@<��hr��c����+1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�33A�33B   B  B  B��B   B(  B0  B8  B?��BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�  B�33B�  B�  B�  B�  B�  C   C�fC�C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~�C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C��3C��3C�  C�  C��C��C�  C��C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C��C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D��D� D  D� D  D� D��D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  Dy�D��D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D��D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1y�D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DE��DF� DG  DG� DH  DHy�DH��DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DM��DN� DOfDO�fDPfDP�fDQ  DQ� DQ��DR� DS  DS� DTfDT�fDUfDU� DV  DV� DV��DW� DX  DX� DX��DYy�DY��DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di�fDjfDj� DkfDk�fDlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~y�D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�3D�C3D��3D��3D�  D�<�D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D��3D��3D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D��3D�3D�C3Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�Dռ�D���D�<�D�|�D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D��3D�3D�C3Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�{A�G�A�z�A�z�A�G�B��B��B=qB��B'��B/��B7��B?=qBG��BO��BW��B`
=Bg��Bo��Bw��B��B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���BӞ�B���B۞�Bߞ�B���B�B���B���B���B���B���B���C�\C�C��C��C	��C��C��C��C��C��C��C�C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[�\C]��C_��Ca��Cc��Cf�Cg��Ci��Ck��Cm��Co��Cq��Cs��Cv�Cx�Cy��C{��C~�C��C��{C��C��C��{C��{C��{C��{C��{C��C��{C��C��{C��{C��C��C��{C��{C�HC�HC��{C�HC�HC��{C��{C��C��C��{C��{C��{C��{C��{C��{C�HC�HC��{C��{C��{C�HC�HC��{C��C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��C��{C�HC��{C��C��{C�HC�HC��{C��{C��{C��{C��C��{C�HC�HC�HC��{C��{C��{C��{C��C��{C��{C��C��C��C��C��C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Ds�D��Dz=D�=Dz=D�=Dz=D��Dz=D�=Dz=D�=D	z=D	�=D
s�D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Ds�D��Dz=D�=Ds�D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D��D �Dz=D��Dz=D�=Dz=D�=Dz=D�=Dz=D �Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#��D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)s�D)�=D*��D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D0 �D0z=D0�=D1s�D1��D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@��D@�=DAz=DA�=DBz=DB�=DCz=DC��DDz=DD�=DEz=DE��DFz=DF�=DGz=DG�=DHs�DH��DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DM �DMz=DM��DNz=DO �DO��DP �DP��DP�=DQz=DQ��DRz=DR�=DSz=DT �DT��DU �DUz=DU�=DVz=DV��DWz=DW�=DXz=DX��DYs�DY��DZz=DZ�=D[z=D\ �D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Dg �Dgz=Dg�=Dhz=Dh�=Di��Dj �Djz=Dk �Dk��Dl �Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~s�D~�=Dz=D�=D�=D�}D��D��D�=D�}D���D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D���D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�y�D��D��D�=D�}D��D� RD�=D�}D��D��D�@RD�}D��D��D�=D�}D��D��D�=D��RD��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D� RD�@RD��RD��RD��D�9�D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D���D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���D��D�=D�}D��D��D�=D�}D��D��D�@RD�}D��D��D�=D��RD��RD��D�=D��RD��RD��D�=D�}D��D��D�=D�}D��D��D�=D�y�D���D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD� RD�@RD�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}D��RD� RD�@RD�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�9�D�y�Dչ�D���D�9�D�y�DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD���D�9�D�}D��RD� RD�@RD�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�RD��RD��D�=D�}D�D��D�=D�}D��RD��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�y�D�D��D�=D�}D�D� RD�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�9�D�}D�D� RD�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D��D��D�=D��RD��RD��D�=D�}D��D��D�&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\A��GA��eA��7A���A��kA��	A���A��A���A�yrA�c A�2-A���A���A�w2A��)A��A��kA��A���A��A��A���A��A��'A��A���A���A�r�A��)A���A�e,A���A��A�$A�hsA��gA��A�"hA�zxA�*�A��A�b�A�h�A��XA�p�A��A��TA���A�(A���A�ϫA���A�A���A��+A�f2A��,A�F�A�SA���A�R�A��A�lWA��vA���A��tA���A�"4A���A��A��A���A��$A��sA��A�k�A��A�]/A��nA�qvA�Z�A�J#A�D3A��A�-CA�͟A�?A��A	A~֡A~�XA~+A}��A|�A{��A{h�Az��Az/�Ay�1Ay_Av��At��At>BAsAq�`Ap&An�}Am�mAl;Ai��Ai�Ah��AhO�Ag�Agr�Ad�AcD�Aa��A_�-A_-A^͟A\�AZ�kAY��AYA�AX�4AWJ#AV�AAU�}AT|AT4AS��AR��AP��AN�AK�AKOAJ��AJsAI��AI2aAI�AHߤAHg�AH�AG�AG�KAG[WAD�AC�AB�AA5?A@TaA?�A?N�A>�A>��A>%A=%�A<�A;k�A:+�A9�A8�rA7�4A6��A4��A3�oA3|�A3	A2�A24A1ZA0��A0�A.�-A-7LA,��A,�LA,�A+�4A*�A*E�A)�xA(�|A(0UA'��A&�+A&�A%�'A%{A$�9A#��A"�A!�	A!bA �#A k�A��A	lA�ACA1�A��A"hA��A�cA��A�'AQA{JA�AAu%AϫA6A�	A!�A�0A?}A��A�AV�A��A�pA�VAhsA
��A
>�A	��A	[�AU�AxlA�A��Ae�A2�A�At�A1'A��Ax�A@�AJ�A�$A~�Al"A B[@�B[@��t@���@�4�@�ȴ@���@��@��'@�z@���@�]�@�7@��@�1�@�L@���@���@�@��@�ߤ@�5�@��y@�]�@��@�]�@���@�u%@���@�^@�]�@�o@�S&@޼j@��@�;�@�+@���@�خ@ժ�@��A@���@�GE@ѿH@��@�bN@���@�S@��@ʰ�@�ߤ@�dZ@ƅ�@�hs@�w�@�+k@���@�X�@�7�@���@���@�_@��@��h@�d�@���@�q@�GE@���@�P�@��*@�.I@��U@�ff@��@��F@�ߤ@���@��@�YK@�<�@��@�oi@��@�&�@�͟@���@�{@��@��[@��~@�c@�"�@��@�V@��@���@���@�6z@�q@��@���@�O�@���@��o@�@���@��8@�z@�	@�:�@�L0@��H@���@�S�@�"�@���@���@�c�@���@��'@�iD@���@��@��@�ϫ@�.I@��@��I@�l"@�GE@�.�@�R�@���@��@�-@��@���@�p�@�C@�Ɇ@�kQ@�~@��#@���@�m]@���@��b@�q�@��o@���@���@�\�@�(�@�ѷ@���@�^5@��
@�@��m@�Q�@�)�@�	@��	@�xl@�J�@�,=@��)@��d@��*@�q@�ں@���@��b@��\@���@���@��K@���@�Y�@�#�@�Ĝ@�l�@�6@�1@�ƨ@���@�j@��@���@�n�@�]d@�  @��M@�B�@��,@��@���@�w�@���@�y>@�	@��@l�@~�@�{@Z�@;d@~�@~��@~��@~3�@}��@}c@}7L@|�$@|��@|�O@|��@{��@{g�@z��@zV@y�=@y5�@y&�@x�@x��@xh�@x/�@w�@w�
@w�P@w8@v҉@vz@v6�@u�@u�"@u�@u��@uf�@u?}@t֡@toi@t4n@t�@s��@sH�@r�@r�@q�X@q&�@p�[@o�@o��@o_p@n��@n�@n_@m�>@m�z@m�@mF@l�/@l�_@l-�@k�r@lG@k\)@j �@h�K@h�@h�o@h_@h4n@g�m@g.I@g�@gv`@g�k@f��@e�@e@@d�5@d��@dɆ@dɆ@d�U@d��@d��@dw�@d�@de�@d]d@dV�@dN�@d-�@d!@d�@c�@c$t@b�@bR�@a��@aO�@a+�@a�@`�@`�D@`  @_E9@^��@^M�@^6�@]�-@\q@[~�@[=@[�@Z�!@Y�3@Y[W@Y<6@Y:�@Y=�@Y/@Y&�@Y�@X�p@X�U@X��@X�Y@XN�@X,=@X-�@X�@W�@W��@V�8@U�@U�3@U��@U�@Uu�@U2a@U \@Toi@S�r@S�@Rxl@R	@Q��@Qw2@Q5�@P��@P��@P��@P�u@P�Y@Pu�@P4n@O�;@O�{@O�@N�@N��@N_�@NGE@M�@M��@M�@Mj@M@@L��@LFt@K�
@K|�@K�@J�@J�L@Js�@J�@I�H@I��@I��@IVm@H�`@H�@G��@G�a@G�@GF�@GY@F�@F�@F�A@FR�@F{@Eϫ@E�@E�@D�@D�@D1@C�}@C��@Ce�@B�y@B�}@B�+@B.�@B_@A��@A��@A�@ADg@@�@@[�@@'R@@�@?�
@?�	@?b�@>�@>��@>��@>d�@>)�@>e@=�@=��@=e,@=V@<|�@<M@;�r@;��@;iD@;F�@;�@:҉@:�R@:��@:&�@9�D@9�d@9s�@9@8�	@8��@8�O@8�_@8tT@84n@8�@8�@7�+@7��@7��@7�k@7t�@7J#@7�@6�@6-@5�=@5\�@55�@4�@4�z@4�_@4��@4h�@4'R@3�@3o�@2�<@2v�@21�@1�D@1��@1o @1�@0�j@0]d@07�@/�]@/�@/S�@/@/@.��@.�,@.W�@.O@-�>@-�@-�H@-�C@-�@-^�@-Dg@-5�@-+�@-&�@-%F@-%F@- \@-@-q@-+@-�@,�@,�@,��@,��@,��@,��@,q@+��@+J#@*��@*	@)�^@)o @(��@(�u@(,=@'�f@'g�@'+@'S@&�@&�,@&~�@&1�@%��@%��@%�-@%�~@%e,@%+@$�@$Ɇ@$l"@$V�@$S�@$K^@$6@#�@#��@#dZ@#1�@#�@"��@"Z�@"�@!��@!��@!o @!c�@!Vm@!Dg@!-w@ ��@ �@ z�@ q@�r@v`@��@R�@-@a|@;�@&�@�@��@�'@��@2a@�f@��@��@�K@Y@��@��@�x@��@h
@L0@3�@)�@e@��@�M@-w@�@�@��@K^@� @x@�4@�{@|�@'�@�@͟@��@ff@-@��@�=@[W@#�@��@��@|�@K^@6@�W@��@@O@��@n�@Q@	@�@ϫ@��@�S@G�@%@�@]d@A�@@�@��@v`@_p@O@.I@�@�@��@�@q�@i�@8�@6�@O@��@��@��@�@^�@Y�@[W@L�@�@�@��@oi@g8@m�@]d@�@b@	�@�@��@��@
�8@
�@
h
@
ff@
V@
e@	�@	�N@	�N@	�d@	�3@	��@	|@	f�@	k�@	s�@	j@	��@	�#@	�j@	�~@	�@��@�	@�E@�)@�@��@��@D�@@��@˒@�P@Z�@S�@1�@�c@҉@͟@�m@��@l�@;�@3�@&�@�@�D@��@��@�n@��@��@��@s�@`B@T�@?}@2a@�@�P@�K@�e@Q�@,=@�@@1@�&@��@��@��@e�@H�@�@�m@�@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\A��GA��eA��7A���A��kA��	A���A��A���A�yrA�c A�2-A���A���A�w2A��)A��A��kA��A���A��A��A���A��A��'A��A���A���A�r�A��)A���A�e,A���A��A�$A�hsA��gA��A�"hA�zxA�*�A��A�b�A�h�A��XA�p�A��A��TA���A�(A���A�ϫA���A�A���A��+A�f2A��,A�F�A�SA���A�R�A��A�lWA��vA���A��tA���A�"4A���A��A��A���A��$A��sA��A�k�A��A�]/A��nA�qvA�Z�A�J#A�D3A��A�-CA�͟A�?A��A	A~֡A~�XA~+A}��A|�A{��A{h�Az��Az/�Ay�1Ay_Av��At��At>BAsAq�`Ap&An�}Am�mAl;Ai��Ai�Ah��AhO�Ag�Agr�Ad�AcD�Aa��A_�-A_-A^͟A\�AZ�kAY��AYA�AX�4AWJ#AV�AAU�}AT|AT4AS��AR��AP��AN�AK�AKOAJ��AJsAI��AI2aAI�AHߤAHg�AH�AG�AG�KAG[WAD�AC�AB�AA5?A@TaA?�A?N�A>�A>��A>%A=%�A<�A;k�A:+�A9�A8�rA7�4A6��A4��A3�oA3|�A3	A2�A24A1ZA0��A0�A.�-A-7LA,��A,�LA,�A+�4A*�A*E�A)�xA(�|A(0UA'��A&�+A&�A%�'A%{A$�9A#��A"�A!�	A!bA �#A k�A��A	lA�ACA1�A��A"hA��A�cA��A�'AQA{JA�AAu%AϫA6A�	A!�A�0A?}A��A�AV�A��A�pA�VAhsA
��A
>�A	��A	[�AU�AxlA�A��Ae�A2�A�At�A1'A��Ax�A@�AJ�A�$A~�Al"A B[@�B[@��t@���@�4�@�ȴ@���@��@��'@�z@���@�]�@�7@��@�1�@�L@���@���@�@��@�ߤ@�5�@��y@�]�@��@�]�@���@�u%@���@�^@�]�@�o@�S&@޼j@��@�;�@�+@���@�خ@ժ�@��A@���@�GE@ѿH@��@�bN@���@�S@��@ʰ�@�ߤ@�dZ@ƅ�@�hs@�w�@�+k@���@�X�@�7�@���@���@�_@��@��h@�d�@���@�q@�GE@���@�P�@��*@�.I@��U@�ff@��@��F@�ߤ@���@��@�YK@�<�@��@�oi@��@�&�@�͟@���@�{@��@��[@��~@�c@�"�@��@�V@��@���@���@�6z@�q@��@���@�O�@���@��o@�@���@��8@�z@�	@�:�@�L0@��H@���@�S�@�"�@���@���@�c�@���@��'@�iD@���@��@��@�ϫ@�.I@��@��I@�l"@�GE@�.�@�R�@���@��@�-@��@���@�p�@�C@�Ɇ@�kQ@�~@��#@���@�m]@���@��b@�q�@��o@���@���@�\�@�(�@�ѷ@���@�^5@��
@�@��m@�Q�@�)�@�	@��	@�xl@�J�@�,=@��)@��d@��*@�q@�ں@���@��b@��\@���@���@��K@���@�Y�@�#�@�Ĝ@�l�@�6@�1@�ƨ@���@�j@��@���@�n�@�]d@�  @��M@�B�@��,@��@���@�w�@���@�y>@�	@��@l�@~�@�{@Z�@;d@~�@~��@~��@~3�@}��@}c@}7L@|�$@|��@|�O@|��@{��@{g�@z��@zV@y�=@y5�@y&�@x�@x��@xh�@x/�@w�@w�
@w�P@w8@v҉@vz@v6�@u�@u�"@u�@u��@uf�@u?}@t֡@toi@t4n@t�@s��@sH�@r�@r�@q�X@q&�@p�[@o�@o��@o_p@n��@n�@n_@m�>@m�z@m�@mF@l�/@l�_@l-�@k�r@lG@k\)@j �@h�K@h�@h�o@h_@h4n@g�m@g.I@g�@gv`@g�k@f��@e�@e@@d�5@d��@dɆ@dɆ@d�U@d��@d��@dw�@d�@de�@d]d@dV�@dN�@d-�@d!@d�@c�@c$t@b�@bR�@a��@aO�@a+�@a�@`�@`�D@`  @_E9@^��@^M�@^6�@]�-@\q@[~�@[=@[�@Z�!@Y�3@Y[W@Y<6@Y:�@Y=�@Y/@Y&�@Y�@X�p@X�U@X��@X�Y@XN�@X,=@X-�@X�@W�@W��@V�8@U�@U�3@U��@U�@Uu�@U2a@U \@Toi@S�r@S�@Rxl@R	@Q��@Qw2@Q5�@P��@P��@P��@P�u@P�Y@Pu�@P4n@O�;@O�{@O�@N�@N��@N_�@NGE@M�@M��@M�@Mj@M@@L��@LFt@K�
@K|�@K�@J�@J�L@Js�@J�@I�H@I��@I��@IVm@H�`@H�@G��@G�a@G�@GF�@GY@F�@F�@F�A@FR�@F{@Eϫ@E�@E�@D�@D�@D1@C�}@C��@Ce�@B�y@B�}@B�+@B.�@B_@A��@A��@A�@ADg@@�@@[�@@'R@@�@?�
@?�	@?b�@>�@>��@>��@>d�@>)�@>e@=�@=��@=e,@=V@<|�@<M@;�r@;��@;iD@;F�@;�@:҉@:�R@:��@:&�@9�D@9�d@9s�@9@8�	@8��@8�O@8�_@8tT@84n@8�@8�@7�+@7��@7��@7�k@7t�@7J#@7�@6�@6-@5�=@5\�@55�@4�@4�z@4�_@4��@4h�@4'R@3�@3o�@2�<@2v�@21�@1�D@1��@1o @1�@0�j@0]d@07�@/�]@/�@/S�@/@/@.��@.�,@.W�@.O@-�>@-�@-�H@-�C@-�@-^�@-Dg@-5�@-+�@-&�@-%F@-%F@- \@-@-q@-+@-�@,�@,�@,��@,��@,��@,��@,q@+��@+J#@*��@*	@)�^@)o @(��@(�u@(,=@'�f@'g�@'+@'S@&�@&�,@&~�@&1�@%��@%��@%�-@%�~@%e,@%+@$�@$Ɇ@$l"@$V�@$S�@$K^@$6@#�@#��@#dZ@#1�@#�@"��@"Z�@"�@!��@!��@!o @!c�@!Vm@!Dg@!-w@ ��@ �@ z�@ q@�r@v`@��@R�@-@a|@;�@&�@�@��@�'@��@2a@�f@��@��@�K@Y@��@��@�x@��@h
@L0@3�@)�@e@��@�M@-w@�@�@��@K^@� @x@�4@�{@|�@'�@�@͟@��@ff@-@��@�=@[W@#�@��@��@|�@K^@6@�W@��@@O@��@n�@Q@	@�@ϫ@��@�S@G�@%@�@]d@A�@@�@��@v`@_p@O@.I@�@�@��@�@q�@i�@8�@6�@O@��@��@��@�@^�@Y�@[W@L�@�@�@��@oi@g8@m�@]d@�@b@	�@�@��@��@
�8@
�@
h
@
ff@
V@
e@	�@	�N@	�N@	�d@	�3@	��@	|@	f�@	k�@	s�@	j@	��@	�#@	�j@	�~@	�@��@�	@�E@�)@�@��@��@D�@@��@˒@�P@Z�@S�@1�@�c@҉@͟@�m@��@l�@;�@3�@&�@�@�D@��@��@�n@��@��@��@s�@`B@T�@?}@2a@�@�P@�K@�e@Q�@,=@�@@1@�&@��@��@��@e�@H�@�@�m@�@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B:�B~B�PB��B�<B�<B�pB�B��B��B�TB��B��B�2B� B��B�Bm�BT�B<�B5�B)yB �B
�B�B �B�B��B��B��B��B�B�B�=B�FB�NB��B��Bz�By�BrBu�B~Bo�BNB;B9$B4�BB�B9�B+�B#�BEB6B�B�JB��B�B�VB�B�)BāB�6B��B�cB��B�BB��B�WB��B�mB{�BwLBu?Bq�BdZB]�BZ7BS�BD�B=�B5%BS�B0�BB	�B-BMBBB5B�B�B�B�BPB3B
��B
��B
�jB
��B
�tB
�?B
�sB
ٚB
��B
��B
�aB
�`B
�B
�B
��B
�dB
��B
�[B
�AB
��B
��B
|�B
q�B
i�B
^�B
ZB
W
B
PB
DB
@ B
=�B
;dB
7�B
2�B
.�B
'�B
$�B
"�B
�B
�B
�B	��B	�cB	�B	�*B	�B	�B	�B	�B	ߤB	��B	�IB	��B	چB	��B	�B	�7B	�B	�aB	��B	��B	��B	�dB	��B	��B	��B	�/B	��B	��B	�jB	�B	�&B	��B	��B	��B	�B	}B	}B	x�B	s�B	p�B	i�B	c:B	b�B	`\B	_�B	\xB	Y�B	YeB	YeB	XEB	S�B	T,B	Q�B	N�B	OBB	O�B	Q4B	O(B	I�B	EmB	BuB	@iB	=�B	:^B	6FB	4�B	/�B	-wB	)yB	'�B	%B	"�B	�B	]B		B	yB	�B	\B	"B	�B	
�B	�B	B	B	�B	 �B�wB��B�DB��B�B�tB�nB�|B�AB�iB��B�B�_B��B��B�8B��B�2B��B�tB�B�nB�HBߤB��B�CB��B�B�_BּB��BԕB�uB�@B��B� B�B��BϑB�<B��B͟B�6B��B�B��BɠB�=B�_B�zB�?B�mB�mB��BāB��BÖB��B��B��B��B�B� B�cB�.B�wB��B��B�"B��B�qB��B��B�dB��B�dB��B�0B�B�B�B�B�qB��B�B��B��B��B�GB��B�3B��BĶB��B�?B�KB�rB��B�~B��B�PB͹B�.BЗBЗB�B�4B� B��B�YB��B�_B�B�VB��B�B�`B�B��B�$B�*B�B��B�}B��B�B��B��B��B��B�6B	�B	AB	�B	GB	MB	�B	oB	�B	�B	�B	�B	 �B	!�B	#�B	&�B	(�B	*�B	-CB	/OB	0oB	4B	7�B	7�B	7�B	8�B	8�B	9�B	<�B	B'B	J�B	M6B	MjB	NVB	P.B	RoB	TB	U�B	WYB	W�B	^B	c�B	gB	g�B	i*B	j�B	kB	kQB	mB	m�B	t9B	w�B	y�B	y�B	~�B	�[B	��B	�7B	��B	��B	��B	��B	��B	�yB	�EB	�_B	�	B	�B	�xB	��B	��B	�B	��B	�RB	�DB	��B	�qB	�}B	�B	�|B	��B	�B	�B	�B	�jB	�HB	��B	�oB	�B	ǔB	ɠB	�jB	ϑB	��B	бB	�B	��B	�B	յB	�?B	چB	�B	�B	��B	��B	� B	�nB	��B	�sB	�yB	�"B	� B	�5B	��B	�B	��B	�2B	��B	�XB	��B	�^B	�B	�6B	��B
�B
9B
YB
	B
�B
�B
jB
�B
�B
pB
�B
�B
{B
B
�B
$B
�B
�B
�B
�B
=B
�B
OB
 'B
"�B
$�B
%�B
(XB
+�B
-wB
.�B
/ B
/�B
0UB
0UB
1'B
4B
5�B
6�B
88B
:^B
=B
>]B
@�B
@�B
@�B
A B
AB
D�B
F%B
GB
J=B
L~B
M�B
Q B
U�B
W�B
YB
Y�B
Y�B
ZB
ZB
ZQB
[�B
\�B
]�B
^OB
`BB
a�B
cB
d&B
dZB
e�B
g�B
hXB
l"B
n�B
p�B
q�B
rB
r�B
s�B
uB
v�B
yXB
z^B
{B
~�B
�GB
��B
��B
�?B
��B
��B
��B
��B
�	B
��B
�	B
�=B
�rB
��B
��B
��B
�~B
��B
��B
��B
��B
�(B
�\B
�NB
��B
��B
��B
�YB
�yB
�B
��B
��B
��B
��B
�HB
�hB
� B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�XB
�*B
�KB
��B
��B
��B
��B
�qB
��B
��B
�)B
��B
��B
�OB
�oB
�[B
��B
�MB
��B
�nB
�tB
��B
��B
�2B
�B
�$B
�B
�B
�dB
�B
��B
�VB
��B
�(B
��B
�B
��B
�OB
� B
�uB
ªB
�-B
�3B
ĜB
��B
��B
��B
�_B
�EB
��B
�KB
ȀB
��B
��B
�#B
˒B
�B
�dB
̳B
�B
�jB
͟B
ΥB
�(B
�(B
ϑB
�B
��B
�}B
� B
�B
уB
ңB
ӏB
��B
ԕB
��B
�B
՛B
�B
�B
�mB
�?B
�YB
��B
�yB
�B
�eB
ٚB
�B
�B
�kB
�	B
�WB
�WB
�WB
ۦB
�B
�)B
ܒB
��B
�B
�dB
�B
߾B
��B
�'B
�B
�-B
�-B
�bB
�B
��B
�B
� B
�B
�tB
�,B
�`B
��B
�LB
��B
�B
�B
�B
�B
�B
��B
�B
�B
�B
��B
�B
�"B
�WB
�qB
�qB
�B
��B
�)B
�]B
�]B
�wB
�wB
�B
�B
��B
��B
��B
��B
��B
��B
�/B
�IB
��B
�B
�B
��B
�B
�UB
�B
�GB
�B
�B
�B
�9B
��B
��B
��B
�+B
�FB
�zB
�zB
�2B
��B
�B
�B
�B
��B
��B
�>B
�rB
��B
�^B
�^B
�DB
�DB
�DB
��B
�dB
��B
�B
�6B
��B
�"B
��B
��B
�BB
��B
��B
��B
��B
��B
��B
�.B
�HB
�.B
��B B�B�B�B[B�B�B{BaBaBGB�B�B�B�B�B�B�B�B�B�B�B�BBBBEB�B�B�B�B�B�B	�B	�B
#B
#B
XB
rB
#B
=B
XB
�B
�B)B^B�B�BBJBBBB�BB�B(BBBvB�B�BBB.B�BNB B B B�B�B&BuBuB�B�B�B,B{B�B�B�B�B�B2BgBBB9B�B�B�B
B�B+B_BEB_B�BBeBKBKB�BkB�B�B	B	B	BWB�B�BB)BCB�B�B~B�B�B�B�B�B�B B �B!HB!HB!HB!�B!�B!�B!�B"B"�B"�B"�B# B#:B#TB#:B#nB$B$&B$&B$&B$ZB$�B$�B$�B$�B%B%,B%zB%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B&B&LB&LB&�B'8B'�B'�B'�B'�B(
B(>B(XB(�B(�B(�B)DB)�B)yB)yB)y44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B:�B~B�PB��B�<B�<B�pB�B��B��B�TB��B��B�2B� B��B�Bm�BT�B<�B5�B)yB �B
�B�B �B�B��B��B��B��B�B�B�=B�FB�NB��B��Bz�By�BrBu�B~Bo�BNB;B9$B4�BB�B9�B+�B#�BEB6B�B�JB��B�B�VB�B�)BāB�6B��B�cB��B�BB��B�WB��B�mB{�BwLBu?Bq�BdZB]�BZ7BS�BD�B=�B5%BS�B0�BB	�B-BMBBB5B�B�B�B�BPB3B
��B
��B
�jB
��B
�tB
�?B
�sB
ٚB
��B
��B
�aB
�`B
�B
�B
��B
�dB
��B
�[B
�AB
��B
��B
|�B
q�B
i�B
^�B
ZB
W
B
PB
DB
@ B
=�B
;dB
7�B
2�B
.�B
'�B
$�B
"�B
�B
�B
�B	��B	�cB	�B	�*B	�B	�B	�B	�B	ߤB	��B	�IB	��B	چB	��B	�B	�7B	�B	�aB	��B	��B	��B	�dB	��B	��B	��B	�/B	��B	��B	�jB	�B	�&B	��B	��B	��B	�B	}B	}B	x�B	s�B	p�B	i�B	c:B	b�B	`\B	_�B	\xB	Y�B	YeB	YeB	XEB	S�B	T,B	Q�B	N�B	OBB	O�B	Q4B	O(B	I�B	EmB	BuB	@iB	=�B	:^B	6FB	4�B	/�B	-wB	)yB	'�B	%B	"�B	�B	]B		B	yB	�B	\B	"B	�B	
�B	�B	B	B	�B	 �B�wB��B�DB��B�B�tB�nB�|B�AB�iB��B�B�_B��B��B�8B��B�2B��B�tB�B�nB�HBߤB��B�CB��B�B�_BּB��BԕB�uB�@B��B� B�B��BϑB�<B��B͟B�6B��B�B��BɠB�=B�_B�zB�?B�mB�mB��BāB��BÖB��B��B��B��B�B� B�cB�.B�wB��B��B�"B��B�qB��B��B�dB��B�dB��B�0B�B�B�B�B�qB��B�B��B��B��B�GB��B�3B��BĶB��B�?B�KB�rB��B�~B��B�PB͹B�.BЗBЗB�B�4B� B��B�YB��B�_B�B�VB��B�B�`B�B��B�$B�*B�B��B�}B��B�B��B��B��B��B�6B	�B	AB	�B	GB	MB	�B	oB	�B	�B	�B	�B	 �B	!�B	#�B	&�B	(�B	*�B	-CB	/OB	0oB	4B	7�B	7�B	7�B	8�B	8�B	9�B	<�B	B'B	J�B	M6B	MjB	NVB	P.B	RoB	TB	U�B	WYB	W�B	^B	c�B	gB	g�B	i*B	j�B	kB	kQB	mB	m�B	t9B	w�B	y�B	y�B	~�B	�[B	��B	�7B	��B	��B	��B	��B	��B	�yB	�EB	�_B	�	B	�B	�xB	��B	��B	�B	��B	�RB	�DB	��B	�qB	�}B	�B	�|B	��B	�B	�B	�B	�jB	�HB	��B	�oB	�B	ǔB	ɠB	�jB	ϑB	��B	бB	�B	��B	�B	յB	�?B	چB	�B	�B	��B	��B	� B	�nB	��B	�sB	�yB	�"B	� B	�5B	��B	�B	��B	�2B	��B	�XB	��B	�^B	�B	�6B	��B
�B
9B
YB
	B
�B
�B
jB
�B
�B
pB
�B
�B
{B
B
�B
$B
�B
�B
�B
�B
=B
�B
OB
 'B
"�B
$�B
%�B
(XB
+�B
-wB
.�B
/ B
/�B
0UB
0UB
1'B
4B
5�B
6�B
88B
:^B
=B
>]B
@�B
@�B
@�B
A B
AB
D�B
F%B
GB
J=B
L~B
M�B
Q B
U�B
W�B
YB
Y�B
Y�B
ZB
ZB
ZQB
[�B
\�B
]�B
^OB
`BB
a�B
cB
d&B
dZB
e�B
g�B
hXB
l"B
n�B
p�B
q�B
rB
r�B
s�B
uB
v�B
yXB
z^B
{B
~�B
�GB
��B
��B
�?B
��B
��B
��B
��B
�	B
��B
�	B
�=B
�rB
��B
��B
��B
�~B
��B
��B
��B
��B
�(B
�\B
�NB
��B
��B
��B
�YB
�yB
�B
��B
��B
��B
��B
�HB
�hB
� B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�XB
�*B
�KB
��B
��B
��B
��B
�qB
��B
��B
�)B
��B
��B
�OB
�oB
�[B
��B
�MB
��B
�nB
�tB
��B
��B
�2B
�B
�$B
�B
�B
�dB
�B
��B
�VB
��B
�(B
��B
�B
��B
�OB
� B
�uB
ªB
�-B
�3B
ĜB
��B
��B
��B
�_B
�EB
��B
�KB
ȀB
��B
��B
�#B
˒B
�B
�dB
̳B
�B
�jB
͟B
ΥB
�(B
�(B
ϑB
�B
��B
�}B
� B
�B
уB
ңB
ӏB
��B
ԕB
��B
�B
՛B
�B
�B
�mB
�?B
�YB
��B
�yB
�B
�eB
ٚB
�B
�B
�kB
�	B
�WB
�WB
�WB
ۦB
�B
�)B
ܒB
��B
�B
�dB
�B
߾B
��B
�'B
�B
�-B
�-B
�bB
�B
��B
�B
� B
�B
�tB
�,B
�`B
��B
�LB
��B
�B
�B
�B
�B
�B
��B
�B
�B
�B
��B
�B
�"B
�WB
�qB
�qB
�B
��B
�)B
�]B
�]B
�wB
�wB
�B
�B
��B
��B
��B
��B
��B
��B
�/B
�IB
��B
�B
�B
��B
�B
�UB
�B
�GB
�B
�B
�B
�9B
��B
��B
��B
�+B
�FB
�zB
�zB
�2B
��B
�B
�B
�B
��B
��B
�>B
�rB
��B
�^B
�^B
�DB
�DB
�DB
��B
�dB
��B
�B
�6B
��B
�"B
��B
��B
�BB
��B
��B
��B
��B
��B
��B
�.B
�HB
�.B
��B B�B�B�B[B�B�B{BaBaBGB�B�B�B�B�B�B�B�B�B�B�B�BBBBEB�B�B�B�B�B�B	�B	�B
#B
#B
XB
rB
#B
=B
XB
�B
�B)B^B�B�BBJBBBB�BB�B(BBBvB�B�BBB.B�BNB B B B�B�B&BuBuB�B�B�B,B{B�B�B�B�B�B2BgBBB9B�B�B�B
B�B+B_BEB_B�BBeBKBKB�BkB�B�B	B	B	BWB�B�BB)BCB�B�B~B�B�B�B�B�B�B B �B!HB!HB!HB!�B!�B!�B!�B"B"�B"�B"�B# B#:B#TB#:B#nB$B$&B$&B$&B$ZB$�B$�B$�B$�B%B%,B%zB%�B%�B%�B%�B%�B%�B%�B%�B%�B%�B&B&LB&LB&�B'8B'�B'�B'�B'�B(
B(>B(XB(�B(�B(�B)DB)�B)yB)yB)y44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604213055  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604225617  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604225617  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604225618                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605075626  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605075626  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220609221504                      G�O�G�O�G�O�                