CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-05T00:35:13Z creation;2016-11-05T00:35:15Z conversion to V3.1;2019-12-19T08:26:14Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20161105003513  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               6A   JA  I2_0576_054                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�׵��� 1   @�׷B^Ѐ@:�����d��}Vl�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D���D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BӞ�B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�\C�\C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D��Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D׀RD׽D��D�=D�}DؽD���D�=D�}DٽD��D�@RD�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D���D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D��RD��RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aҙ�A�~�A�|�A�x�A�v�A�t�A�r�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�x�A�x�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�~�A�~�A�~�AҁAҁAҁA�~�A�|�A�z�A�x�A�p�A�XA�{A�7LA�;dA���A�$�A�bA�ĜA��A��A�&�A�5?A�~�A��A���A�ĜA���A�A�A�9XA��A�`BA�ȴA��A���A���A�|�A�oA�XA�&�A��`A��jA�;dA���A�l�A�A�M�A�G�A���A��A��A��
A��mA�/A�=qA��#A�1A��PA�^5A���A��A�M�A���A�-A�C�A�n�A�$�A���A���A��A�VA���A�A�A��A��#A�?}A�XA�7A~-A|�Az��Ay�FAyXAx��Ax1Aw%Av  As��Ar��Ao&�Am�Al�Ak|�AkVAj~�Ai�mAh�/AehsAc�mAbVA`9XA_�FA]�mA\ZA[K�AZ�`AZ1'A[x�A]A]�AZ��AX�AWdZAVv�AV�yAWO�AV��AV��AU��AT��ATA�AS�AQXAPVAOp�AL��AK�AK?}AJ-AIt�AGt�AF5?AE�mAE�hAD��AC/AB��AB  AAhsA@�RA?|�A>��A>=qA>JA=�A=ƨA=�A=�A<z�A<JA;�TA;��A;
=A: �A9�mA9t�A8Q�A7�7A6ȴA6{A5�hA4��A3G�A1�A1&�A0JA/XA/
=A.~�A-�A,��A,�RA,�A,^5A,=qA,1'A+�FA*VA(��A&JA%��A%dZA%\)A%dZA%O�A%%A$��A#A#dZA#oA" �A!/A �`A�A�A=qA�9A��A�A�A�AXAjAbA�7AȴA�-A1A�wAt�AoA{AA^5A��A\)A5?A
ĜA��A�A��A�`A�DA�A�wA&�A�jA1A�FA�hAXA �R@�S�@��\@���@�bN@�ƨ@�o@���@��@�x�@�+@�@�A�@�ff@�X@�9@�C�@��@�u@�33@��T@���@� �@�ƨ@�t�@�K�@��y@��@�M�@��@�h@�7L@��u@�A�@��m@�33@ܣ�@�$�@ٲ-@٩�@���@֧�@���@�bN@�
=@��@љ�@�G�@��`@�r�@�Q�@�  @�M�@ʰ!@�V@�@�&�@���@��@�ƨ@��^@�(�@�ff@�@���@���@�n�@��@��D@��w@��@�J@���@�x�@�&�@��@�9X@��+@�p�@��j@��@���@�t�@�;d@�+@�@��u@��@���@�@��`@�A�@���@�dZ@�ȴ@��^@���@�bN@��
@��!@��!@��@��H@�M�@���@�&�@��@� �@���@�-@�$�@�J@��T@���@���@��h@��@��@��@��H@�{@��#@�G�@�bN@��@�l�@�E�@��@�hs@�V@���@�z�@���@���@�l�@�@���@���@��+@�^5@�@�O�@���@��D@��u@�Q�@��@��y@���@�E�@��T@���@��@��D@��D@���@�z�@�bN@�bN@� �@��
@�dZ@�K�@�o@���@�^5@�E�@���@��@���@�?}@��@��/@��j@��9@��9@��@���@��D@�j@��@�ƨ@�dZ@�o@�@��@��y@��H@��+@�$�@�{@��@�@���@��h@�`B@��`@���@��@�z�@�j@�9X@�;@|�@~ȴ@}��@}�@|�@|j@{�@{"�@z�\@y�#@y�^@x��@xA�@w�;@w|�@w
=@v�R@u`B@t��@t9X@st�@s"�@s@r�!@rJ@q��@qx�@qhs@qhs@q%@p��@p1'@o\)@o+@o
=@nȴ@n��@nE�@nE�@nV@n{@n@m�@m`B@mV@l�/@lz�@lz�@lZ@lz�@l�j@mV@m`B@m/@m/@m/@m�@m�@mV@mV@l��@l��@l�@lj@k33@jM�@i�^@i��@iG�@h��@hb@hQ�@h  @g�@gK�@f�R@f$�@e��@e�@e�@d�j@d�@d��@d��@dz�@c�m@c�F@c��@cdZ@cC�@co@b��@b�@a�^@aG�@`r�@`1'@`b@`  @_\)@^ȴ@^�+@^{@^@]�T@]�T@]��@]`B@]�@]V@]V@]V@]V@\�@\�j@\��@\Z@\(�@[��@[��@[dZ@["�@Z�@Z~�@Y��@Y�7@Y7L@Xr�@W��@V��@V�R@VV@V$�@V@V@U�T@U�-@U?}@T�@Tj@T1@S�F@SC�@S@R�\@R^5@R=q@Q��@Q��@Q��@QG�@Q&�@P��@P�u@PA�@Pb@O�;@O�;@O|�@O�@N�R@N�+@N{@M��@Mp�@MO�@L��@Lz�@L1@K�m@K�F@Kt�@J�@J-@I��@I��@IG�@IG�@I7L@I&�@H��@H��@H�@HQ�@G��@G��@G�@G�P@Gl�@Gl�@G�@F�R@F�+@Fv�@FE�@E@E`B@E�@D��@D�D@D�@C��@C�
@C��@CC�@C33@C33@C@B�!@B�\@Bn�@A��@A�^@A��@Ax�@Ahs@Ahs@AG�@A�@@��@@��@@��@@�u@@bN@@b@?l�@>�R@>5?@=@=`B@=�@<��@<�@<�@<Z@;��@;33@:�H@:��@:��@:�!@:�\@:-@9��@9hs@9G�@9&�@9%@8��@8�u@8r�@81'@7�;@7��@7K�@7�@6ȴ@6V@6$�@6@5�@5�@5/@4�/@4��@4�@4z�@4I�@3dZ@3@2�@2��@2��@2^5@1�#@1X@0��@0��@0r�@0A�@/�w@/K�@.ȴ@.�+@.E�@-�@-p�@-O�@-?}@-V@,�j@,��@,Z@+��@+��@+S�@+o@*��@*n�@*�@)��@)�#@)��@)��@)��@)hs@)7L@)�@(��@(Ĝ@(�@(Q�@(A�@(  @(  @'�;@'��@'�@'�P@'|�@'\)@'+@&�y@&��@&��@&��@&V@%�T@%��@%�@$�j@$(�@#��@#�
@#��@#t�@#C�@#"�@"��@"~�@"n�@"=q@!��@!��@!x�@!hs@!G�@!�@ �`@ ��@ �@ bN@ Q�@ b@��@��@\)@�@��@v�@�@��@��@?}@��@��@z�@(�@(�@�@1@�
@��@��@S�@"�@�@�H@��@�\@^5@�@��@��@%@�9@�@�@�u@b@�@�@�@ȴ@�R@�R@��@��@��@�+@v�@ff@v�@v�@v�@�+@v�@v�@v�@ff@V@5?@{@��@p�@O�@?}@/@�@V@��@�D@��@S�@"�@@��@�\@^5@^5@^5@=q@�@�#@��@hs@G�@��@Ĝ@r�@bN@Q�@1'@�@��@��@�@K�@�@�y@ȴ@��@V@�@�h@�h@�h@�@p�@O�@?}@�@��@�@�D@Z@9X@(�@�F@C�@@
�@
��@
�!@
�\@
M�@
�@	�^@	��@	��@	��@	��@	�7@	��@	x�@	%@��@�u@Q�@A�@b@�@�;@�;@�w@�@��@�P@�P@�P@�P@K�@;d@�@
=@��@
=@�y@ȴ@�R@��@�+@ff@ff@V@5?@@��@��@��@��@�h@�@p�@`B@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aҙ�A�~�A�|�A�x�A�v�A�t�A�r�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�x�A�x�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�~�A�~�A�~�AҁAҁAҁA�~�A�|�A�z�A�x�A�p�A�XA�{A�7LA�;dA���A�$�A�bA�ĜA��A��A�&�A�5?A�~�A��A���A�ĜA���A�A�A�9XA��A�`BA�ȴA��A���A���A�|�A�oA�XA�&�A��`A��jA�;dA���A�l�A�A�M�A�G�A���A��A��A��
A��mA�/A�=qA��#A�1A��PA�^5A���A��A�M�A���A�-A�C�A�n�A�$�A���A���A��A�VA���A�A�A��A��#A�?}A�XA�7A~-A|�Az��Ay�FAyXAx��Ax1Aw%Av  As��Ar��Ao&�Am�Al�Ak|�AkVAj~�Ai�mAh�/AehsAc�mAbVA`9XA_�FA]�mA\ZA[K�AZ�`AZ1'A[x�A]A]�AZ��AX�AWdZAVv�AV�yAWO�AV��AV��AU��AT��ATA�AS�AQXAPVAOp�AL��AK�AK?}AJ-AIt�AGt�AF5?AE�mAE�hAD��AC/AB��AB  AAhsA@�RA?|�A>��A>=qA>JA=�A=ƨA=�A=�A<z�A<JA;�TA;��A;
=A: �A9�mA9t�A8Q�A7�7A6ȴA6{A5�hA4��A3G�A1�A1&�A0JA/XA/
=A.~�A-�A,��A,�RA,�A,^5A,=qA,1'A+�FA*VA(��A&JA%��A%dZA%\)A%dZA%O�A%%A$��A#A#dZA#oA" �A!/A �`A�A�A=qA�9A��A�A�A�AXAjAbA�7AȴA�-A1A�wAt�AoA{AA^5A��A\)A5?A
ĜA��A�A��A�`A�DA�A�wA&�A�jA1A�FA�hAXA �R@�S�@��\@���@�bN@�ƨ@�o@���@��@�x�@�+@�@�A�@�ff@�X@�9@�C�@��@�u@�33@��T@���@� �@�ƨ@�t�@�K�@��y@��@�M�@��@�h@�7L@��u@�A�@��m@�33@ܣ�@�$�@ٲ-@٩�@���@֧�@���@�bN@�
=@��@љ�@�G�@��`@�r�@�Q�@�  @�M�@ʰ!@�V@�@�&�@���@��@�ƨ@��^@�(�@�ff@�@���@���@�n�@��@��D@��w@��@�J@���@�x�@�&�@��@�9X@��+@�p�@��j@��@���@�t�@�;d@�+@�@��u@��@���@�@��`@�A�@���@�dZ@�ȴ@��^@���@�bN@��
@��!@��!@��@��H@�M�@���@�&�@��@� �@���@�-@�$�@�J@��T@���@���@��h@��@��@��@��H@�{@��#@�G�@�bN@��@�l�@�E�@��@�hs@�V@���@�z�@���@���@�l�@�@���@���@��+@�^5@�@�O�@���@��D@��u@�Q�@��@��y@���@�E�@��T@���@��@��D@��D@���@�z�@�bN@�bN@� �@��
@�dZ@�K�@�o@���@�^5@�E�@���@��@���@�?}@��@��/@��j@��9@��9@��@���@��D@�j@��@�ƨ@�dZ@�o@�@��@��y@��H@��+@�$�@�{@��@�@���@��h@�`B@��`@���@��@�z�@�j@�9X@�;@|�@~ȴ@}��@}�@|�@|j@{�@{"�@z�\@y�#@y�^@x��@xA�@w�;@w|�@w
=@v�R@u`B@t��@t9X@st�@s"�@s@r�!@rJ@q��@qx�@qhs@qhs@q%@p��@p1'@o\)@o+@o
=@nȴ@n��@nE�@nE�@nV@n{@n@m�@m`B@mV@l�/@lz�@lz�@lZ@lz�@l�j@mV@m`B@m/@m/@m/@m�@m�@mV@mV@l��@l��@l�@lj@k33@jM�@i�^@i��@iG�@h��@hb@hQ�@h  @g�@gK�@f�R@f$�@e��@e�@e�@d�j@d�@d��@d��@dz�@c�m@c�F@c��@cdZ@cC�@co@b��@b�@a�^@aG�@`r�@`1'@`b@`  @_\)@^ȴ@^�+@^{@^@]�T@]�T@]��@]`B@]�@]V@]V@]V@]V@\�@\�j@\��@\Z@\(�@[��@[��@[dZ@["�@Z�@Z~�@Y��@Y�7@Y7L@Xr�@W��@V��@V�R@VV@V$�@V@V@U�T@U�-@U?}@T�@Tj@T1@S�F@SC�@S@R�\@R^5@R=q@Q��@Q��@Q��@QG�@Q&�@P��@P�u@PA�@Pb@O�;@O�;@O|�@O�@N�R@N�+@N{@M��@Mp�@MO�@L��@Lz�@L1@K�m@K�F@Kt�@J�@J-@I��@I��@IG�@IG�@I7L@I&�@H��@H��@H�@HQ�@G��@G��@G�@G�P@Gl�@Gl�@G�@F�R@F�+@Fv�@FE�@E@E`B@E�@D��@D�D@D�@C��@C�
@C��@CC�@C33@C33@C@B�!@B�\@Bn�@A��@A�^@A��@Ax�@Ahs@Ahs@AG�@A�@@��@@��@@��@@�u@@bN@@b@?l�@>�R@>5?@=@=`B@=�@<��@<�@<�@<Z@;��@;33@:�H@:��@:��@:�!@:�\@:-@9��@9hs@9G�@9&�@9%@8��@8�u@8r�@81'@7�;@7��@7K�@7�@6ȴ@6V@6$�@6@5�@5�@5/@4�/@4��@4�@4z�@4I�@3dZ@3@2�@2��@2��@2^5@1�#@1X@0��@0��@0r�@0A�@/�w@/K�@.ȴ@.�+@.E�@-�@-p�@-O�@-?}@-V@,�j@,��@,Z@+��@+��@+S�@+o@*��@*n�@*�@)��@)�#@)��@)��@)��@)hs@)7L@)�@(��@(Ĝ@(�@(Q�@(A�@(  @(  @'�;@'��@'�@'�P@'|�@'\)@'+@&�y@&��@&��@&��@&V@%�T@%��@%�@$�j@$(�@#��@#�
@#��@#t�@#C�@#"�@"��@"~�@"n�@"=q@!��@!��@!x�@!hs@!G�@!�@ �`@ ��@ �@ bN@ Q�@ b@��@��@\)@�@��@v�@�@��@��@?}@��@��@z�@(�@(�@�@1@�
@��@��@S�@"�@�@�H@��@�\@^5@�@��@��@%@�9@�@�@�u@b@�@�@�@ȴ@�R@�R@��@��@��@�+@v�@ff@v�@v�@v�@�+@v�@v�@v�@ff@V@5?@{@��@p�@O�@?}@/@�@V@��@�D@��@S�@"�@@��@�\@^5@^5@^5@=q@�@�#@��@hs@G�@��@Ĝ@r�@bN@Q�@1'@�@��@��@�@K�@�@�y@ȴ@��@V@�@�h@�h@�h@�@p�@O�@?}@�@��@�@�D@Z@9X@(�@�F@C�@@
�@
��@
�!@
�\@
M�@
�@	�^@	��@	��@	��@	��@	�7@	��@	x�@	%@��@�u@Q�@A�@b@�@�;@�;@�w@�@��@�P@�P@�P@�P@K�@;d@�@
=@��@
=@�y@ȴ@�R@��@�+@ff@ff@V@5?@@��@��@��@��@�h@�@p�@`B@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBŢB��B��B��B�B�B�)B�/B�mB�B�BB�HB�B�B�B��B��B�B�B�sB�`B�BB�HB�B��B��BƨB�wB�qB�dB�XB�-B��B��B�=Bp�B[#BJ�B49B�B%B�B�yB�BB��B�dB�3B��B��B��B�oB�PB�Bz�BhsBdZB]/BS�BF�B;dB49B)�B �BB
��B
}�B
%�B
\B
  B	��B	�B	�mB	�sB	�fB	�5B	�
B	ǮB	�wB	�B	��B	��B	�hB	�PB	�=B	�B	~�B	l�B	]/B	T�B	F�B	A�B	>wB	33B	/B	2-B	5?B	T�B	� B	��B	v�B	^5B	J�B	9XB	W
B	m�B	u�B	}�B	�B	x�B	t�B	q�B	e`B	[#B	VB	D�B	>wB	;dB	6FB	2-B	)�B	"�B	 �B	�B	�B	JB	%B	B	B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�mB�TB�HB�;B�/B�)B�B�
B�
B��B��B��B��BɺBȴBǮBƨBŢBĜBĜB�qB�RB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�DB�7B�B|�Bz�Bw�Br�Bl�Bk�BjBhsBhsBaHB_;B]/B\)BYBVBR�BQ�BO�BM�BI�BE�B@�B?}B>wB<jB;dB6FB49B49B2-B1'B0!B/B/B-B+B+B(�B'�B'�B&�B$�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B&�B&�B&�B)�B)�B-B/B0!B2-B2-B2-B33B33B33B5?B:^B;dB?}BA�BC�BI�BL�BM�BN�BP�BS�BVBXB[#B`BBaHBffBhsBn�Bu�Bx�Bx�Bw�Bw�Bx�B{�B�B�B�B�B�%B�B�B�B�B�%B�+B�7B�VB�VB�\B�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�LB�wB�}BÖBǮBȴB��B��B�B�`B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	+B		7B	DB	JB	JB	JB	JB	PB	PB	\B	uB	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	'�B	(�B	)�B	+B	,B	/B	33B	33B	49B	6FB	7LB	9XB	=qB	?}B	B�B	H�B	J�B	L�B	M�B	R�B	S�B	VB	XB	XB	\)B	_;B	aHB	bNB	dZB	ffB	k�B	l�B	n�B	r�B	t�B	u�B	w�B	x�B	{�B	}�B	�B	�B	�%B	�+B	�7B	�JB	�PB	�PB	�VB	�VB	�VB	�\B	�hB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�-B	�-B	�-B	�-B	�3B	�3B	�3B	�3B	�9B	�RB	�^B	�dB	�jB	�jB	�jB	�jB	��B	B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�5B	�BB	�HB	�NB	�TB	�TB	�ZB	�fB	�mB	�sB	�yB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
PB
VB
VB
\B
hB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
+B
+B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
m�B
m�B
n�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��BǮBǮBǮBǮBǮBǮBǮBǮBǮB��BǮBǮBǮBǮBǮBǮBǮB��BǮB��BǮBǮBǮBǮBǮBǮBǮB��B��B�KB�lB��B�dB�B�mB�xB�/B��B�;B�KB� B�:B�`B�}B�UB�/B��B��B��B��B�KB�B�ZB�BچB�
BбB��B�.B�B�6B�JB��B�sB��B��Bu?B_!BNVB88B!�B
	B��B�"B�B�gB�<B��B�>B�B�YB��B��B�B|�Bi�Be�B^�BVBHfB<�B5�B+�B#�B	�B
�7B
��B
($B
�B
�B	��B	�B	�$B	��B	��B	��B	�eB	��B	�[B	�]B	��B	�SB	� B	�<B	�^B	�EB	��B	n�B	_VB	W?B	G�B	C�B	@B	4TB	/�B	2aB	4B	SuB	�;B	�1B	y$B	_�B	K�B	9$B	V�B	nB	vFB	HB	�AB	y�B	u�B	t�B	f�B	\�B	X�B	E�B	?�B	<�B	7�B	4TB	+QB	#nB	!�B	�B	xB	B	+B	B	-B�wB��B�dB�*B�$B�>B�XB��B��B�FB�B�TB�B��B�5B�wB�B�B�B�ZB�NB�B�;B��B�YB�_B��BԕB��B�B̈́B�#B�B�B��B�BżBƨB��B�B��B�KB�B�B�DB��B��B�$B�nB��B�B��B�_B��B��B��B�jB�^B�AB}�B|By�Bs�Bm]BlqBk�BjBjKBa�B_�B^B]�BZ�BV�BS�BR�BQ�BPBLBF�BA;B@�B?}B>B=�B72B4�B5%B2�B1�B0�B0;B0UB-�B+�B+�B)�B(�B)_B(XB&LB$tB#B�B�BdBCB�B�B�B�BBQBB�B�B�B�B�BBBB�BB�B�ByBKBBBB�B/B�B=B�B�B 'B!B;B;BB�BpB!-B�B$&B�B�B1B�B	B�B�BQB�BqB�B�B \B \B!|B"�B'8B'B'mB*eB*�B./B0B0�B2�B2�B2|B3�B3�B4B6FB;0B<6B@BB[BDMBJ=BMBN�BO�BQ�BT{BV�BX�B[=B`BBa�BgBi*Bo BvFBy�By�BxBw�By$B|6B�;B�oB�MB��B��B��B��B��B�mB��B��B��B��B�(B��B��B��B��B��B��B��B��B�B�B�B�B�,B�fB�_B�kB�]B�OB�vB��B��B��B��B��B��B�7B�VB�B�1B�B�B�B��B��B��B��B�%B�LB�6B�(B�HB	 B	uB	aB	_B		lB	xB	~B	~B	dB	dB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	# B	'B	(>B	)DB	*0B	+6B	,WB	/iB	3hB	3�B	4�B	6`B	7�B	9�B	=�B	?�B	CB	IB	KB	MB	N<B	S&B	TFB	VmB	X+B	XyB	\]B	_pB	a�B	b�B	d�B	f�B	k�B	l�B	n�B	r�B	t�B	vB	xB	x�B	|B	~B	� B	�MB	�YB	��B	��B	�dB	�jB	�jB	��B	��B	�pB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�'B	�AB	�GB	�GB	�GB	�GB	�MB	�MB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�.B	� B	� B	� B	�B	�,B	�FB	�9B	�YB	�EB	�eB	�=B	�WB	�]B	�jB	��B	�bB	�B	�nB	�nB	�tB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	�%B	�B	�8B	��B	��B	�B	�B	��B	�B	�B	�<B	�(B	�.B
'B
AB
aB
3B
mB
?B
?B
EB
_B
_B
_B
fB
fB
�B
	RB

rB

rB
^B
�B
xB
dB
dB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#B
# B
$@B
%,B
%,B
&B
%�B
'B
'B
($B
(
B
)*B
)*B
)DB
+6B
+B
+B
+6B
+B
+6B
+QB
,"B
,=B
-CB
-CB
-)B
-CB
-CB
.IB
./B
.IB
/5B
/OB
/5B
0UB
0UB
0UB
1AB
1[B
1�B
2GB
2GB
3hB
3hB
3hB
3�B
4nB
5ZB
5ZB
5ZB
5�B
5�B
6zB
7�B
8�B
8lB
9�B
9�B
:�B
:�B
;�B
;B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
KB
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
NB
M�B
NB
M�B
OB
OB
OB
PB
O�B
P.B
Q B
Q B
QB
QB
RB
RB
RB
RB
Q�B
RB
R B
RB
SB
S&B
SB
SB
TB
S&B
SB
T,B
T,B
T,B
TFB
UMB
UB
VB
VB
VB
V9B
WYB
WYB
YKB
YB
Y1B
YB
YB
Y1B
Y1B
YB
Y1B
YB
Y1B
Y1B
YB
YB
YB
YB
Y1B
Y1B
YKB
ZQB
ZQB
Z7B
ZQB
[#B
[#B
[=B
[#B
[WB
[=B
[qB
\�B
]dB
]dB
^OB
^jB
^jB
^OB
^5B
^5B
^OB
^OB
_VB
_VB
_VB
_pB
_VB
`\B
`\B
`\B
`\B
`\B
abB
a|B
aHB
abB
abB
abB
abB
abB
b�B
b�B
cnB
cnB
dZB
dZB
dZB
dtB
dtB
dZB
dtB
dtB
d�B
e�B
ezB
ezB
e�B
e�B
f�B
f�B
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
gmB
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
iyB
i�B
i�B
i�B
j�B
jB
jB
jB
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
m�B
m�B
n�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611090038292016110900382920161109003829201806221216232018062212162320180622121623201804050409172018040504091720180405040917  JA  ARFMdecpA19c                                                                20161105093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161105003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161105003513  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161105003514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161105003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161105003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161105003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161105003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161105003515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161105003515                      G�O�G�O�G�O�                JA  ARUP                                                                        20161105013306                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161105153607  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161108153829  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161108153829  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190917  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031623  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                