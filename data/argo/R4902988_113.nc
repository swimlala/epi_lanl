CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-14T03:49:14Z creation;2022-11-14T03:49:15Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221114034914  20221114040251  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               qA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���v#�1   @����Z��@;E`A�7L�dn��O�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   AA��A`  A�  A�33A�33A�33A�  A�  A�33A�33B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C!�fC#�fC&  C(  C)�fC+�fC-�fC0  C2  C3�fC6  C8  C9�fC<  C>�C@  CB  CD�CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz�C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C��3C��3D   D � DfD� D  D� D  D�fDfD�fD  D� D  D� D��D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D�fDfD�fDfD� D  D� D  D� D  D�fDfD�fD  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DBfDB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDHfDH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Duy�Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dy��Dzy�D{  D{� D|  D|� D|��D}� D~  D~� D  D� D�  D�@ D�� D�� D���D�@ D�� D�� D�3D�@ D��3D��3D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�|�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�� D���D���D�<�D�|�D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D���D�  D�@ D��3D��3D�  D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�C3D̀ D�� D�  D�C3D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ D�|�D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�<�Dހ D�� D�  D�@ D߀ D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D���D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�C3D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@��@��A�\A@(�A^�\A~�\A�z�A�z�A�z�A�G�A�G�A�z�A�z�A�G�B��B��B
=B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C�C��C��C��C��C��C��C��C��C��C!�\C#�\C%��C'��C)�\C+�\C-�\C/��C1��C3�\C5��C7��C9�\C;��C>�C?��CA��CD�CE��CG��CI�\CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cx�Cz�C{��C}��C��C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C�HC��{C��C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��C��{C��{C��{C��{C��{C�HC�HC�HC��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��C��{C�HC��{C��{C�HC��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C�HC�HC�HC��{C��{C��{C��{C��{C��C��C��{D z=D �Dz=D�=Dz=D�=D��D �D��D�=Dz=D�=Dz=D��Dz=D�=Dz=D�=D	z=D	�=D
z=D
��Dz=D�=Dz=D�=Dz=D�=Dz=D�=D��D �D��D �Dz=D�=Dz=D�=Dz=D�=D��D �D��D�=Dz=D�=Dz=D �Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D# �D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8��D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>��D>�=D?z=D?�=D@z=D@�=DAz=DB �DB��DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DG��DH �DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DL �DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DT��DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY��DZz=D[ �D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbs�Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt��Dus�Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy��Dzs�Dz�=D{z=D{�=D|z=D|��D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D���D�=D�}D��D� RD�=D��RD��RD��D�=D�}D���D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D� RD�=D�}D��D��D�=D�y�D���D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�9�D�}D���D���D�9�D�y�D��D��D�@RD��RD��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���D���D�=D�}D��D��D�=D�}D��D��D�=D�y�D���D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�9�D�y�D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D���D��D�=D��RD��RD��D�=D�}D��RD� RD�@RD�}D��D��D�=D�}D��D��D�@RD��RD��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD���D�=D�}DǽD� RD�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD���D�=D�}D˽D��D�@RD�}D̽D��D�@RD�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�y�D۽D��D�=D�}DܽD��D�=D�}DݽD��D�9�D�}D޽D��D�=D�}D߹�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�@RD�}D�D���D�=D�}D�D���D�=D�}D�D��D�=D�}D��D��D�=D�}D�D� RD�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D��D��D�=D�y�D�D��D�=D�}D�D��D�=D�}D�D��D�9�D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�@RD��RD��D��D�=D�y�D��D��D�=D�}D��D��D�9�D�}D��D��D�@RD�}D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aˌ~Aˏ�A˘�A˛	A˙eA˛�A˚�A˙�A˙�A˛qA˚A˘+A˒:Aˎ�AˍAˋ�Aˉ�A˄MA˅�AˈfAˉ�Aˇ�Aˈ�Aˆ�A˅SAˆ�Aˆ�Aˇ�Aˈ1Aˇ�Aˉ7AˊrAˋDAˌAˍ�Aˍ�Aˍ�AˎVAˊ=Aˍ�A��Aȩ�A�՛A�>wA�zDA�DA�� A���A�V9A���A���A��A�NA��uA�xA���A�A�\�A��?A��]A�E�A��FA�HA��A��rA���A�?}A��A���A��A�w�A�|A�"A��-A��A��KA�P�A�J�A�OBA��A��|A�e�A��A��BA��A�n/A�4A�Q�A�یA��uA�O�A��5A�!-A���A���A���A�,A��nA�^5A��A��A�CaA���A�z�A�Q�A�#A��\A���A��A��A��A���A��A~S�A}��A}4AyD�Av��Au�At��At6As�Asc Ar�
Ar�"Aq�jAo}�An��An�Al�dAh�oAfc�Ae��Ae��AeJAc�OAbi�A`0�A^�aA\��A[)_AZ{AW��AWx�AWm]AU��AR�AQe,AO�KAM�jAK��AK��AI�CAF�AF��AF��AFJAE6AC#�A@e�A>�\A>�A<�A;ZA9��A8��A7��A7~A6�kA6�.A6�uA5c A3�]A2�.A2W�A0R�A-�_A,v`A+�A)�|A(��A'�HA&�pA&Y�A%��A%MA%DgA$|�A#B[A"�BA"o�A"J#A"=�A"<6A"4�A!�AA �AqAJ�A�A�KA>BA��A�4AC�Ad�Af�A��Ah
AU�A� Ay>A@AK�AخA#:A��Av`A�A�}A)�A�A
�|A	��A	�A<�A+�A��A��A�:A�kA^5A%FA(A�9A:*A��A��A;�A�NAffAϫAf�@�.I@���@���@�(@�a@��@�ԕ@���@���@� \@�X@���@�\�@�^�@�L@��@��@�͟@�d�@�.�@�	�@�7@�M�@�C@�>�@��E@�g8@�p�@�_@���@ޒ�@�9X@�zx@�C�@۴�@���@���@ٔ�@�a@�{J@�dZ@֛�@�0�@Ӳ�@��3@�I�@�(�@��@�~�@���@�j�@̯O@�%�@˳�@���@�@ɐ�@�A�@��@�}V@��>@Ƿ@Ƴh@�u�@�h
@�S�@�N�@�O@��9@�l�@��2@�ȴ@Ĉ�@��@��?@���@���@�\�@��@�[W@��@��j@��o@�\�@��@�˒@� i@�:�@�a�@���@���@��@�q@�33@��.@���@�|�@�G�@��@��8@���@��@�h
@��+@��e@�8�@�*�@��@�ԕ@���@�_p@��@��@��@���@��m@��.@�n/@�4@�(@��5@���@�֡@��)@���@���@���@�H�@�@O@��@��@�Ĝ@��z@�� @�tT@�GE@���@��@��@��o@�Ft@�G�@�z@��t@�\�@�G�@��p@�tT@��@�1@���@��@���@���@��W@���@�8�@�8�@�˒@�_�@�I�@��o@���@��\@�W�@�4n@�2�@�(�@��@���@�e�@���@��.@�:�@��$@�Ĝ@���@�Z@�1'@��@���@���@��V@�x�@�Vm@�L�@�+@��@���@�?�@���@�a�@��@���@�S�@��@��@�G@��A@��@���@�}�@�/@��@��8@���@�?@�@��3@�ԕ@��m@��@��h@���@���@�x�@�4@�,�@��8@��z@���@�PH@�	@��@��P@�Ĝ@�~�@�Q�@�1'@��@�ϫ@�N<@��"@���@F�@~.�@|�4@{�K@{��@{�$@{~�@{j�@{X�@{H�@{H�@{�@z�2@z�b@y�.@yG�@yA @y�@x�_@xH@xb@w��@w�@w��@w��@v��@v��@v�+@vV@v{@u7L@sخ@s��@sg�@s)_@s�@so@r�2@r�m@r	@q�@qp�@q@p~(@p/�@o�;@oJ#@n��@n�R@nB[@n!�@n�@m�@m��@mq@lی@l��@l��@k�;@k@j;�@iԕ@h�@h!@h	�@g��@g�g@g�a@g�w@g��@g��@g�@fH�@f3�@f0U@f@e��@e��@e^�@e�@e;@d�P@d�[@d<�@c� @c>�@b�'@b0U@a�d@ak�@`�@`�@`r�@`U2@`:�@`�@_�W@_�q@^p;@^u@]�j@]�@]�@]��@]��@]��@]w2@]-w@\�K@\~(@[�
@[v`@Z��@Ze@Y�@Y@Y�S@Y/@X�p@X|�@X$@W��@W�*@W��@WU�@W,�@Vv�@U��@U@T�E@T�@TD�@S��@R�@R�@Q��@Q\�@QS&@QF@Q?}@Q(�@Q@Q�@P�	@P�`@P�[@P�[@P��@P�u@P��@O�@O��@O�@N��@N�x@N��@N��@Nc @NB[@N#:@M��@M��@M��@M�@L��@L�D@L7�@L1@K� @K��@K�a@K��@K��@K�P@K~�@K>�@K�@J��@J��@IVm@H%�@G��@G$t@F�@F{�@FJ�@F{@Eϫ@E�@E��@E��@E�@E��@Ej@D�@DtT@D>B@D$@D�@D�@D�@C��@C�Q@C��@C!-@C�@B�@Bu@A�-@A�h@A�h@A��@Ac@AL�@A/@A	l@@��@@��@@~(@@j@@D�@@(�@@M@?��@?�6@?+@>l�@>$�@=�@=�@=ϫ@=�"@=k�@=%F@<�_@<PH@<C-@<x@;�r@;�m@;��@;~�@;�@:�H@:�'@:��@:H�@9�N@9�n@9u�@9/@8r�@7�@7Y@6�L@6Z�@6H�@6#:@5�o@5�X@5m]@50�@4�	@4��@3�@3�Q@3�@3��@3'�@3@3
=@2�@2�m@2��@2$�@1��@1�-@1%F@0�@0oi@0C-@0-�@0G@/��@/,�@.�@.�x@.}V@..�@-�j@-w2@-@,�)@,��@,�@,9X@+��@+�;@+�
@+�k@+U�@+9�@+1�@++@+)_@+$t@+(@+
=@*�@*�b@)�@(�@(��@([�@(M@'�&@'��@'��@'��@'dZ@'S�@'_p@'P�@'"�@'@&�x@&_@%�C@%X@$ی@$e�@$�@#�w@#��@#33@"�@"ں@"�B@"��@"s�@!�D@!�@!��@!�h@!:�@ ��@ �e@ �_@ �@ g8@ PH@�&@��@��@�:@��@�	@��@|�@x@o�@dZ@K�@&@�@�@d�@!�@�C@��@�n@�'@�"@|@!�@��@��@N�@9X@1'@"h@@G@�]@�Q@�k@qv@ں@��@i�@:*@e@J@�@��@�@�M@X@q@�v@tT@7�@�]@��@=@"�@�@�@��@��@V@0U@O@��@ԕ@��@L�@�@�@@�@%@�5@�@�v@��@��@��@'R@�@�A@�g@�w@��@O@&@��@v�@4@��@�@�S@�"@X@��@*�@�+@ݘ@��@��@�@U�@9�@)_@�"@��@�"@�8@��@~�@?@��@��@`B@ی@��@z�@�@��@��@y�@dZ@]�@W?@O@Mj@J#@4�@
��@
�,@
q�@
#:@	�)@	��@	rG@	e,@	^�@	Dg@	�@�K@��@�@z�@/�@M@��@��@8@�@�@�@��@+�@�@��@e�@Q�@,=@�@�@��@��@��@j�@F�@9�@�@�@�y@�@��@�'@�<@�h@��@��@�r@�A@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aˌ~Aˏ�A˘�A˛	A˙eA˛�A˚�A˙�A˙�A˛qA˚A˘+A˒:Aˎ�AˍAˋ�Aˉ�A˄MA˅�AˈfAˉ�Aˇ�Aˈ�Aˆ�A˅SAˆ�Aˆ�Aˇ�Aˈ1Aˇ�Aˉ7AˊrAˋDAˌAˍ�Aˍ�Aˍ�AˎVAˊ=Aˍ�A��Aȩ�A�՛A�>wA�zDA�DA�� A���A�V9A���A���A��A�NA��uA�xA���A�A�\�A��?A��]A�E�A��FA�HA��A��rA���A�?}A��A���A��A�w�A�|A�"A��-A��A��KA�P�A�J�A�OBA��A��|A�e�A��A��BA��A�n/A�4A�Q�A�یA��uA�O�A��5A�!-A���A���A���A�,A��nA�^5A��A��A�CaA���A�z�A�Q�A�#A��\A���A��A��A��A���A��A~S�A}��A}4AyD�Av��Au�At��At6As�Asc Ar�
Ar�"Aq�jAo}�An��An�Al�dAh�oAfc�Ae��Ae��AeJAc�OAbi�A`0�A^�aA\��A[)_AZ{AW��AWx�AWm]AU��AR�AQe,AO�KAM�jAK��AK��AI�CAF�AF��AF��AFJAE6AC#�A@e�A>�\A>�A<�A;ZA9��A8��A7��A7~A6�kA6�.A6�uA5c A3�]A2�.A2W�A0R�A-�_A,v`A+�A)�|A(��A'�HA&�pA&Y�A%��A%MA%DgA$|�A#B[A"�BA"o�A"J#A"=�A"<6A"4�A!�AA �AqAJ�A�A�KA>BA��A�4AC�Ad�Af�A��Ah
AU�A� Ay>A@AK�AخA#:A��Av`A�A�}A)�A�A
�|A	��A	�A<�A+�A��A��A�:A�kA^5A%FA(A�9A:*A��A��A;�A�NAffAϫAf�@�.I@���@���@�(@�a@��@�ԕ@���@���@� \@�X@���@�\�@�^�@�L@��@��@�͟@�d�@�.�@�	�@�7@�M�@�C@�>�@��E@�g8@�p�@�_@���@ޒ�@�9X@�zx@�C�@۴�@���@���@ٔ�@�a@�{J@�dZ@֛�@�0�@Ӳ�@��3@�I�@�(�@��@�~�@���@�j�@̯O@�%�@˳�@���@�@ɐ�@�A�@��@�}V@��>@Ƿ@Ƴh@�u�@�h
@�S�@�N�@�O@��9@�l�@��2@�ȴ@Ĉ�@��@��?@���@���@�\�@��@�[W@��@��j@��o@�\�@��@�˒@� i@�:�@�a�@���@���@��@�q@�33@��.@���@�|�@�G�@��@��8@���@��@�h
@��+@��e@�8�@�*�@��@�ԕ@���@�_p@��@��@��@���@��m@��.@�n/@�4@�(@��5@���@�֡@��)@���@���@���@�H�@�@O@��@��@�Ĝ@��z@�� @�tT@�GE@���@��@��@��o@�Ft@�G�@�z@��t@�\�@�G�@��p@�tT@��@�1@���@��@���@���@��W@���@�8�@�8�@�˒@�_�@�I�@��o@���@��\@�W�@�4n@�2�@�(�@��@���@�e�@���@��.@�:�@��$@�Ĝ@���@�Z@�1'@��@���@���@��V@�x�@�Vm@�L�@�+@��@���@�?�@���@�a�@��@���@�S�@��@��@�G@��A@��@���@�}�@�/@��@��8@���@�?@�@��3@�ԕ@��m@��@��h@���@���@�x�@�4@�,�@��8@��z@���@�PH@�	@��@��P@�Ĝ@�~�@�Q�@�1'@��@�ϫ@�N<@��"@���@F�@~.�@|�4@{�K@{��@{�$@{~�@{j�@{X�@{H�@{H�@{�@z�2@z�b@y�.@yG�@yA @y�@x�_@xH@xb@w��@w�@w��@w��@v��@v��@v�+@vV@v{@u7L@sخ@s��@sg�@s)_@s�@so@r�2@r�m@r	@q�@qp�@q@p~(@p/�@o�;@oJ#@n��@n�R@nB[@n!�@n�@m�@m��@mq@lی@l��@l��@k�;@k@j;�@iԕ@h�@h!@h	�@g��@g�g@g�a@g�w@g��@g��@g�@fH�@f3�@f0U@f@e��@e��@e^�@e�@e;@d�P@d�[@d<�@c� @c>�@b�'@b0U@a�d@ak�@`�@`�@`r�@`U2@`:�@`�@_�W@_�q@^p;@^u@]�j@]�@]�@]��@]��@]��@]w2@]-w@\�K@\~(@[�
@[v`@Z��@Ze@Y�@Y@Y�S@Y/@X�p@X|�@X$@W��@W�*@W��@WU�@W,�@Vv�@U��@U@T�E@T�@TD�@S��@R�@R�@Q��@Q\�@QS&@QF@Q?}@Q(�@Q@Q�@P�	@P�`@P�[@P�[@P��@P�u@P��@O�@O��@O�@N��@N�x@N��@N��@Nc @NB[@N#:@M��@M��@M��@M�@L��@L�D@L7�@L1@K� @K��@K�a@K��@K��@K�P@K~�@K>�@K�@J��@J��@IVm@H%�@G��@G$t@F�@F{�@FJ�@F{@Eϫ@E�@E��@E��@E�@E��@Ej@D�@DtT@D>B@D$@D�@D�@D�@C��@C�Q@C��@C!-@C�@B�@Bu@A�-@A�h@A�h@A��@Ac@AL�@A/@A	l@@��@@��@@~(@@j@@D�@@(�@@M@?��@?�6@?+@>l�@>$�@=�@=�@=ϫ@=�"@=k�@=%F@<�_@<PH@<C-@<x@;�r@;�m@;��@;~�@;�@:�H@:�'@:��@:H�@9�N@9�n@9u�@9/@8r�@7�@7Y@6�L@6Z�@6H�@6#:@5�o@5�X@5m]@50�@4�	@4��@3�@3�Q@3�@3��@3'�@3@3
=@2�@2�m@2��@2$�@1��@1�-@1%F@0�@0oi@0C-@0-�@0G@/��@/,�@.�@.�x@.}V@..�@-�j@-w2@-@,�)@,��@,�@,9X@+��@+�;@+�
@+�k@+U�@+9�@+1�@++@+)_@+$t@+(@+
=@*�@*�b@)�@(�@(��@([�@(M@'�&@'��@'��@'��@'dZ@'S�@'_p@'P�@'"�@'@&�x@&_@%�C@%X@$ی@$e�@$�@#�w@#��@#33@"�@"ں@"�B@"��@"s�@!�D@!�@!��@!�h@!:�@ ��@ �e@ �_@ �@ g8@ PH@�&@��@��@�:@��@�	@��@|�@x@o�@dZ@K�@&@�@�@d�@!�@�C@��@�n@�'@�"@|@!�@��@��@N�@9X@1'@"h@@G@�]@�Q@�k@qv@ں@��@i�@:*@e@J@�@��@�@�M@X@q@�v@tT@7�@�]@��@=@"�@�@�@��@��@V@0U@O@��@ԕ@��@L�@�@�@@�@%@�5@�@�v@��@��@��@'R@�@�A@�g@�w@��@O@&@��@v�@4@��@�@�S@�"@X@��@*�@�+@ݘ@��@��@�@U�@9�@)_@�"@��@�"@�8@��@~�@?@��@��@`B@ی@��@z�@�@��@��@y�@dZ@]�@W?@O@Mj@J#@4�@
��@
�,@
q�@
#:@	�)@	��@	rG@	e,@	^�@	Dg@	�@�K@��@�@z�@/�@M@��@��@8@�@�@�@��@+�@�@��@e�@Q�@,=@�@�@��@��@��@j�@F�@9�@�@�@�y@�@��@�'@�<@�h@��@��@�r@�A@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B��B�`B�,B�B�`B�`B�FB�FB�FB�`B�zB��B��B��B�fB�fB�RB�8B��B�RB��B��B��B��B�B�DB�_B��B��B��B�B�eB�eB�B��B��B�6B�6B��B�CBi�B�#B��B��B}�Bw�BuZBrBs�BmCBh�Bc�B^B[�BZQBYBVBU�BQ�BN�BL�BI�B=qB:�B33B/ B-�B,"B*eB(XBjB�B�qB�B�B�gB��B�0B��B~�Bu�Bq�BnIBi�Bf�Bb�BXBO(BJ#BD�B@ B3�B B�B�B�B B��B��B�TB�?B�5B��B��B�|B�#B��By�BZ�BN<B:DB.IB$tB�B�BB
�AB
��B
�|B
�B
�CB
��B
ԕB
�&B
��B
�GB
�DB
�FB
��B
�eB
�=B
�tB
�MB
��B
y�B
rGB
d�B
^�B
QhB
J�B
F�B
8�B
6`B
4�B
.�B
�B
B
VB
�B	��B	��B	��B	��B	�|B	�pB	�dB	��B	��B	�mB	�lB	��B	��B	��B	�
B	�'B	��B	��B	�B	�
B	�9B	�
B	��B	�=B	��B	�iB	u�B	oOB	j�B	cnB	_pB	\xB	X+B	V9B	S�B	R�B	Q4B	PbB	LB	J#B	IB	HB	G�B	G�B	F�B	E�B	B�B	>BB	9�B	8lB	7�B	6FB	4B	3�B	1[B	1[B	,WB	+B	'�B	'�B	 B	�B	�B	�B	uB	B	�B	�B	(B	6B	^B		�B		�B	�B	MB	�B	�B	B	'B	oB	UB	�B	 �B	 �B	 �B	  B�HB��B��B��B��B��B�*B�B�`B��B��B��B�?B��B�B��B�FB�B�B�'B��B�B�B��B��B�B��B�oB�B�B�B�B�oB��B�5B�GB�TB�B�B�B��B�TB�tB��B�B��B��B��B��B�8B�XB��B�VB��B�B�(B��B�.B	 B	 4B	 OB	�B	�B	�B	�B	�B	?B	�B	B	
=B		�B	
#B	
=B	
#B	
�B	DB	�B	�B	�B	6B	�B	�B	�B	?B	�B	7B	�B	xB	B	~B	/B	!-B	# B	%B	)B	/�B	4�B	7�B	9�B	?�B	C{B	E�B	G�B	I�B	JrB	K)B	K^B	LdB	L�B	MB	N�B	T,B	UB	UMB	U�B	V�B	WYB	W�B	YKB	]/B	bB	e�B	i�B	k�B	m�B	m�B	ncB	n�B	o B	n�B	oB	o B	o5B	p;B	q�B	u�B	y�B	z�B	{0B	{B	{B	z�B	y�B	z�B	��B	�gB	��B	�9B	��B	��B	��B	�B	��B	��B	�B	�.B	�.B	�.B	�}B	�}B	�bB	��B	�\B	��B	�B	�B	�vB	��B	��B	�,B	��B	��B	��B	��B	�B	�qB	��B	��B	�zB	�RB	�JB	��B	ǔB	ȀB	�=B	��B	ΥB	��B	�HB	ЗB	ѷB	��B	�@B	�FB	�9B	خB	ܬB	�-B	�8B	�B	�iB	�B	�B	��B	�B	�?B	��B	��B	��B	�dB	�B	��B	�B	�B	�B	�B
 B
�B
aB
B
	B
	7B
	lB
DB
�B
VB
�B
�B
B
[B
{B
�B
�B
�B
 B
 �B
!B
"4B
#�B
%zB
)_B
/�B
2GB
6�B
9>B
9�B
9�B
:^B
:�B
:�B
;B
;JB
<jB
=<B
>BB
?.B
AB
@�B
BB
DMB
E�B
F�B
HfB
H�B
IB
I�B
K�B
MB
MjB
M�B
N"B
QhB
U�B
V�B
WYB
W�B
W�B
X+B
X�B
YB
\xB
]dB
_;B
`�B
c B
dB
e`B
f�B
g�B
h
B
i�B
jB
jeB
kB
k�B
mwB
n/B
n/B
n�B
pB
raB
t�B
u�B
y	B
{�B
|B
|�B
|�B
|�B
}B
}�B
}�B
�B
��B
�B
�gB
��B
�+B
�fB
�B
��B
��B
��B
�B
�JB
�jB
��B
��B
��B
�2B
�mB
��B
��B
�B
�kB
��B
�#B
�qB
�B
��B
��B
��B
��B
�NB
��B
��B
��B
�B
�nB
��B
��B
�mB
�DB
�WB
�]B
��B
��B
�B
�5B
��B
��B
��B
��B
�TB
�9B
�TB
�B
�B
��B
��B
��B
��B
��B
�0B
�<B
��B
�;B
�B
�AB
�uB
B
��B
�-B
ÖB
��B
�B
�MB
�MB
��B
�B
��B
��B
��B
�B
ɆB
ɺB
��B
�	B
ʌB
�)B
˒B
��B
�JB
�~B
��B
�VB
�B
ϑB
��B
�HB
�bB
�}B
бB
��B
��B
� B
уB
ѷB
�oB
�TB
ՁB
רB
��B
��B
�QB
�WB
یB
�)B
��B
��B
��B
��B
�B
�/B
�IB
��B
ߊB
��B
�'B
�B
�'B
�\B
�vB
��B
��B
��B
��B
�B
�&B
�B
��B
��B
�B
�B
�`B
�B
��B
�B
��B
��B
�B
�mB
�B
�B
�B
�
B
�yB
��B
�6B
�B
�B
�B
�WB
�qB
��B
�B
�B
�}B
�B
�5B
�OB
�B
�;B
��B
�'B
�[B
�AB
�GB
�B
�hB
�B
��B
�%B
��B
�fB
�8B
�lB
��B
��B
�	B
��B
��B
�^B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�(B
�wB
��B
�HB
�cB �BBUB�B�B�B�BGB�BBB�BB�B?B�B�BBzB�BBBfB�B	B	B	B	B	B	RB	7B	RB	�B
�B0BdBBPB�B�B�B�BBBB"B<B<B�B(B�BB�B�B B�B�B&B�B�B�B�B,B�B�B�B�B�B
B$B?BYBsB�BEB�B�B�B�B�B�B�B�B�B�BBKB�BB7B�BWBWB=BWBqBWB�BB�BB/BB/BIBIB/BdB~B�B�B5B�B�BBB�B�B�B�B B \B �B!HB!|B!�B"hB#B#B# B# B#:B#�B$@B$ZB$tB$�B$�B$�B%�B&2B&B&2B&fB&2B&fB&fB&LB&�B&�B'B'�B'�B'�B'�B(
B(XB(�B(�B)_B)�B*0B*KB*�B*�B*�B+B,WB,�B,�B,�B-)B-CB-wB-�B-�B-�B./B.B./B./B.cB.�B/5B/�B0B0;B1B1'B1[B2GB2|B2�B2�B3B3B33B3B33B33B3MB3�B3�B4TB4�B5B5�B5�B5�B5�B6+B6FB6`B6zB6�B6�B72B7B7LB8B8lB9>B9�B9�B:^B;B;B<B<PB<jB<�B<�B<�B<�B="B=qB=�B=�B=�B=�B>(B>BB>]B>]B>�B>wB>�B>wB>�B>�B>�B>�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B�B��B�`B�,B�B�`B�`B�FB�FB�FB�`B�zB��B��B��B�fB�fB�RB�8B��B�RB��B��B��B��B�B�DB�_B��B��B��B�B�eB�eB�B��B��B�6B�6B��B�CBi�B�#B��B��B}�Bw�BuZBrBs�BmCBh�Bc�B^B[�BZQBYBVBU�BQ�BN�BL�BI�B=qB:�B33B/ B-�B,"B*eB(XBjB�B�qB�B�B�gB��B�0B��B~�Bu�Bq�BnIBi�Bf�Bb�BXBO(BJ#BD�B@ B3�B B�B�B�B B��B��B�TB�?B�5B��B��B�|B�#B��By�BZ�BN<B:DB.IB$tB�B�BB
�AB
��B
�|B
�B
�CB
��B
ԕB
�&B
��B
�GB
�DB
�FB
��B
�eB
�=B
�tB
�MB
��B
y�B
rGB
d�B
^�B
QhB
J�B
F�B
8�B
6`B
4�B
.�B
�B
B
VB
�B	��B	��B	��B	��B	�|B	�pB	�dB	��B	��B	�mB	�lB	��B	��B	��B	�
B	�'B	��B	��B	�B	�
B	�9B	�
B	��B	�=B	��B	�iB	u�B	oOB	j�B	cnB	_pB	\xB	X+B	V9B	S�B	R�B	Q4B	PbB	LB	J#B	IB	HB	G�B	G�B	F�B	E�B	B�B	>BB	9�B	8lB	7�B	6FB	4B	3�B	1[B	1[B	,WB	+B	'�B	'�B	 B	�B	�B	�B	uB	B	�B	�B	(B	6B	^B		�B		�B	�B	MB	�B	�B	B	'B	oB	UB	�B	 �B	 �B	 �B	  B�HB��B��B��B��B��B�*B�B�`B��B��B��B�?B��B�B��B�FB�B�B�'B��B�B�B��B��B�B��B�oB�B�B�B�B�oB��B�5B�GB�TB�B�B�B��B�TB�tB��B�B��B��B��B��B�8B�XB��B�VB��B�B�(B��B�.B	 B	 4B	 OB	�B	�B	�B	�B	�B	?B	�B	B	
=B		�B	
#B	
=B	
#B	
�B	DB	�B	�B	�B	6B	�B	�B	�B	?B	�B	7B	�B	xB	B	~B	/B	!-B	# B	%B	)B	/�B	4�B	7�B	9�B	?�B	C{B	E�B	G�B	I�B	JrB	K)B	K^B	LdB	L�B	MB	N�B	T,B	UB	UMB	U�B	V�B	WYB	W�B	YKB	]/B	bB	e�B	i�B	k�B	m�B	m�B	ncB	n�B	o B	n�B	oB	o B	o5B	p;B	q�B	u�B	y�B	z�B	{0B	{B	{B	z�B	y�B	z�B	��B	�gB	��B	�9B	��B	��B	��B	�B	��B	��B	�B	�.B	�.B	�.B	�}B	�}B	�bB	��B	�\B	��B	�B	�B	�vB	��B	��B	�,B	��B	��B	��B	��B	�B	�qB	��B	��B	�zB	�RB	�JB	��B	ǔB	ȀB	�=B	��B	ΥB	��B	�HB	ЗB	ѷB	��B	�@B	�FB	�9B	خB	ܬB	�-B	�8B	�B	�iB	�B	�B	��B	�B	�?B	��B	��B	��B	�dB	�B	��B	�B	�B	�B	�B
 B
�B
aB
B
	B
	7B
	lB
DB
�B
VB
�B
�B
B
[B
{B
�B
�B
�B
 B
 �B
!B
"4B
#�B
%zB
)_B
/�B
2GB
6�B
9>B
9�B
9�B
:^B
:�B
:�B
;B
;JB
<jB
=<B
>BB
?.B
AB
@�B
BB
DMB
E�B
F�B
HfB
H�B
IB
I�B
K�B
MB
MjB
M�B
N"B
QhB
U�B
V�B
WYB
W�B
W�B
X+B
X�B
YB
\xB
]dB
_;B
`�B
c B
dB
e`B
f�B
g�B
h
B
i�B
jB
jeB
kB
k�B
mwB
n/B
n/B
n�B
pB
raB
t�B
u�B
y	B
{�B
|B
|�B
|�B
|�B
}B
}�B
}�B
�B
��B
�B
�gB
��B
�+B
�fB
�B
��B
��B
��B
�B
�JB
�jB
��B
��B
��B
�2B
�mB
��B
��B
�B
�kB
��B
�#B
�qB
�B
��B
��B
��B
��B
�NB
��B
��B
��B
�B
�nB
��B
��B
�mB
�DB
�WB
�]B
��B
��B
�B
�5B
��B
��B
��B
��B
�TB
�9B
�TB
�B
�B
��B
��B
��B
��B
��B
�0B
�<B
��B
�;B
�B
�AB
�uB
B
��B
�-B
ÖB
��B
�B
�MB
�MB
��B
�B
��B
��B
��B
�B
ɆB
ɺB
��B
�	B
ʌB
�)B
˒B
��B
�JB
�~B
��B
�VB
�B
ϑB
��B
�HB
�bB
�}B
бB
��B
��B
� B
уB
ѷB
�oB
�TB
ՁB
רB
��B
��B
�QB
�WB
یB
�)B
��B
��B
��B
��B
�B
�/B
�IB
��B
ߊB
��B
�'B
�B
�'B
�\B
�vB
��B
��B
��B
��B
�B
�&B
�B
��B
��B
�B
�B
�`B
�B
��B
�B
��B
��B
�B
�mB
�B
�B
�B
�
B
�yB
��B
�6B
�B
�B
�B
�WB
�qB
��B
�B
�B
�}B
�B
�5B
�OB
�B
�;B
��B
�'B
�[B
�AB
�GB
�B
�hB
�B
��B
�%B
��B
�fB
�8B
�lB
��B
��B
�	B
��B
��B
�^B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�(B
�wB
��B
�HB
�cB �BBUB�B�B�B�BGB�BBB�BB�B?B�B�BBzB�BBBfB�B	B	B	B	B	B	RB	7B	RB	�B
�B0BdBBPB�B�B�B�BBBB"B<B<B�B(B�BB�B�B B�B�B&B�B�B�B�B,B�B�B�B�B�B
B$B?BYBsB�BEB�B�B�B�B�B�B�B�B�B�BBKB�BB7B�BWBWB=BWBqBWB�BB�BB/BB/BIBIB/BdB~B�B�B5B�B�BBB�B�B�B�B B \B �B!HB!|B!�B"hB#B#B# B# B#:B#�B$@B$ZB$tB$�B$�B$�B%�B&2B&B&2B&fB&2B&fB&fB&LB&�B&�B'B'�B'�B'�B'�B(
B(XB(�B(�B)_B)�B*0B*KB*�B*�B*�B+B,WB,�B,�B,�B-)B-CB-wB-�B-�B-�B./B.B./B./B.cB.�B/5B/�B0B0;B1B1'B1[B2GB2|B2�B2�B3B3B33B3B33B33B3MB3�B3�B4TB4�B5B5�B5�B5�B5�B6+B6FB6`B6zB6�B6�B72B7B7LB8B8lB9>B9�B9�B:^B;B;B<B<PB<jB<�B<�B<�B<�B="B=qB=�B=�B=�B=�B>(B>BB>]B>]B>�B>wB>�B>wB>�B>�B>�B>�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221114034858  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221114034914  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221114034914  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221114034915                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221114124919  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221114124919  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221114040251                      G�O�G�O�G�O�                