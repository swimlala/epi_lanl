CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-07-15T09:01:06Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܴ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220715090106  20220715090106  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��V�^z�1   @��Wm�@�@'�     �dn� ě�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B���BÙ�B�  B�  B�33B�  B���B�  B�  B���B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@��@��A�\A>�\A^�\A~�\A�z�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B�B�B���B���B���B�k�B�k�B���B���B�B���Bמ�B���B���B㞹B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2��D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D���D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�S�A�S�A�VA�XA�ZA�\)A�ZA�VA�VA�S�A�XA�XA�VA�Q�A�S�A�S�A�E�A�+A�+A��A�"�A�$�A�&�A�-A�33A�9XA�;dA�?}A�?}A�?}A�=qA�1'A�{A�
=A���A��TA���A���A�ĜAٰ!A�=qA���A�G�A��;AɼjA��A�ȴA�M�A�jA��A��A�K�A�33A�G�A���A�
=A��wA���A���A��A��A�hsA�=qA|�uAy��As�;Am|�Ak�Aj�yAi��Ae;dA_AZ�!AU`BAQ�APM�ANz�AK�#AK/AKoAJ9XAH��AG&�ADVAA�A>�HA=��A:��A8��A8A7��A7`BA57LA5�7A5�#A5dZA4jA3t�A3�A3
=A1��A01A/ƨA/�wA-�TA+�;A+VA)C�A(M�A(1A'A'p�A&��A&��A$n�A#��A#hsA"Q�A!/A!G�A!�FA"ffA"�DA"��A"��A"^5A!�mA!��A!��A!�A!&�A VA �A�wAp�A�hA�A�AVAM�A�AXAoA��A��AbNA �AO�AoAAĜA��AjA�A�
AXA�+A�#AƨAhsAA�\AƨA�AdZA?}A��A�9A�A��A��A�An�AffA9XA�A��A�^A��A�/Az�AI�AJA�;A��AhsA�HA�A^5A1'A-A$�A��A�`A�DAv�AI�A(�A��A�mA��A`BA33A
ĜA
�A
bNA
A�A
 �A
A	��A	�A	�A	ƨA	p�A	33A�A�AA�^A�-A�A�`AA�AhsA�/A5?AƨA�AC�A��A��A��AI�A��AA ��A ��A j@���@�dZ@�33@��!@�J@���@�?}@���@�Z@��w@�dZ@���@�E�@��@���@�V@��@��@�1'@��;@���@�|�@�;d@���@�=q@��-@���@��H@���@�%@�@��@�@��@���@@�t�@�S�@�"�@��H@�~�@�O�@� �@�|�@�+@�?}@��@�Q�@�1'@���@��
@�"�@�`B@�9X@�w@�o@�5?@��`@��u@�  @ߕ�@�
=@ޟ�@�ff@���@�@�G�@ܓu@� �@ۅ@�v�@�-@�-@�@�7L@��@�%@ش9@�r�@ו�@�C�@�hs@Դ9@�I�@ӥ�@���@�V@��T@�`B@���@�z�@�9X@�b@ϕ�@�@�p�@�V@�9X@˅@��@ʧ�@�E�@�O�@�Q�@�|�@�33@�"�@�o@��@Ə\@�V@�{@ź^@Ł@�O�@ě�@�A�@�|�@���@���@¸R@�@+@�~�@�^5@�{@��`@�Z@�1@��;@��w@���@�+@��@���@���@��7@��@��D@�I�@�(�@���@���@���@�l�@�C�@��H@�{@�?}@�z�@�  @���@�"�@���@�J@��^@��@�O�@��@��j@�z�@�A�@�\)@�ff@�J@���@��-@�p�@��@�z�@�Q�@� �@��
@���@�C�@�E�@��-@�`B@�?}@��@���@���@���@���@�V@��@�@���@�X@�Ĝ@�A�@� �@��
@��@�;d@��@�V@��^@��`@��D@�I�@��;@�l�@��H@�5?@��T@�7L@��@�bN@�A�@��F@���@��\@��+@�~�@�~�@�v�@�~�@�v�@�J@��h@�V@��9@�j@�9X@��F@��R@�@�@�hs@��j@�(�@��;@��P@�dZ@��@��R@�~�@�^5@�-@��@�@�O�@�V@��9@��D@�z�@�j@�Z@�I�@��@���@��m@�ƨ@�K�@�"�@�o@��y@���@�^5@�M�@�$�@���@�p�@��j@��D@�A�@�b@��m@�ƨ@���@�K�@�@��y@���@�v�@�n�@�5?@�$�@�@���@��@�X@��`@�z�@��m@�K�@��H@���@�n�@�-@��T@��7@�X@��`@���@��@��F@��@�l�@�33@��+@�E�@�-@�@���@��h@�hs@�G�@��`@��m@�t�@�+@��R@�n�@��@�hs@�?}@���@��@���@��@�@�P@�P@~ff@}�T@}�h@|�j@|(�@{��@{33@z�H@z=q@yx�@y�@xbN@xb@w��@v��@u�T@u�h@uV@t��@tz�@tj@tZ@tI�@s�
@st�@s"�@r�@r��@r��@rM�@q�@qx�@q&�@p��@p�9@pr�@pQ�@pA�@p �@o�w@oK�@o
=@n��@n��@n�@n��@n5?@m�@m�h@m�@l�D@k�
@kC�@j�!@j~�@jn�@j=q@jJ@i��@iX@hĜ@hA�@g�w@g\)@f��@fV@e��@e`B@e�@d�/@d�@d9X@c��@c�@cC�@c"�@co@b�@b��@b��@a�^@`r�@` �@_�@_��@_
=@^�+@^V@^5?@^{@]�@]�h@]`B@]/@\�/@\�D@\I�@[�m@[t�@[33@["�@Z�@Z�H@Z�!@Z^5@Y��@Y�^@Y�7@Y�7@Yx�@Yhs@YX@Y7L@X��@X�@W�@W;d@V��@Vȴ@V�R@V�+@VE�@U�T@Up�@UO�@U/@UV@T�j@Sƨ@S�@St�@SdZ@S33@Rn�@Q��@Q��@Q��@Q��@QG�@P�`@Pr�@Pb@O�w@O|�@Ol�@N��@Nff@N{@M��@M@M?}@Lz�@L1@Kt�@Ko@J�!@J�\@JM�@J�@JJ@I�#@Ix�@Ihs@IG�@H�9@Hb@G�w@G|�@G\)@G;d@Fȴ@F$�@E�h@E`B@D�D@C"�@B�@B�\@B^5@BM�@B=q@B-@BJ@A��@Ahs@@��@@�@@�@@�@@bN@@A�@@b@?��@?�@>�+@=�T@<�@<�@<j@<Z@<(�@;t�@;33@;"�@;o@:�\@:-@9�@9X@9�@8�`@8�9@8�@8Q�@8A�@81'@7�w@6�y@6��@6v�@6ff@6E�@6$�@5�-@5�@5V@4��@4I�@3�
@3t�@3S�@3C�@333@3"�@3o@3@2�H@2�\@2M�@1��@1X@17L@1&�@1�@1%@1%@0��@0Ĝ@0��@0�u@0 �@/�;@/��@/;d@.�y@.�R@.V@.@-�-@-`B@-?}@,��@,�/@,�j@,�D@,I�@+��@+o@*��@*��@*~�@)��@)hs@)&�@(��@(Ĝ@(bN@'��@'|�@'
=@&ȴ@&��@&$�@&@%�T@%�-@%`B@%�@$�@$9X@#dZ@#@"��@"~�@"�@!��@!G�@!&�@!&�@ ��@ �9@ Q�@�@|�@�@ȴ@��@��@v�@E�@@�-@p�@�@��@�@�@�/@Z@�
@dZ@C�@33@�@��@��@~�@-@��@��@�@��@�9@bN@ �@�P@+@
=@
=@��@��@��@v�@ff@ff@ff@V@$�@�@�T@��@�-@�h@`B@/@�@��@�j@��@z�@z�@I�@9X@1@�m@�F@��@S�@@�H@��@�\@=q@�@�^@x�@hs@hs@X@G�@&�@%@%@��@��@�@A�@ �@b@�;@��@�@|�@\)@l�@\)@;d@+@
=@
=@�@�@�@�@ȴ@�R@�R@��@v�@{@@�T@��@�@p�@`B@O�@?}@�@��@��@��@�@z�@I�@9X@�@1@�
@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�S�A�S�A�S�A�VA�XA�ZA�\)A�ZA�VA�VA�S�A�XA�XA�VA�Q�A�S�A�S�A�E�A�+A�+A��A�"�A�$�A�&�A�-A�33A�9XA�;dA�?}A�?}A�?}A�=qA�1'A�{A�
=A���A��TA���A���A�ĜAٰ!A�=qA���A�G�A��;AɼjA��A�ȴA�M�A�jA��A��A�K�A�33A�G�A���A�
=A��wA���A���A��A��A�hsA�=qA|�uAy��As�;Am|�Ak�Aj�yAi��Ae;dA_AZ�!AU`BAQ�APM�ANz�AK�#AK/AKoAJ9XAH��AG&�ADVAA�A>�HA=��A:��A8��A8A7��A7`BA57LA5�7A5�#A5dZA4jA3t�A3�A3
=A1��A01A/ƨA/�wA-�TA+�;A+VA)C�A(M�A(1A'A'p�A&��A&��A$n�A#��A#hsA"Q�A!/A!G�A!�FA"ffA"�DA"��A"��A"^5A!�mA!��A!��A!�A!&�A VA �A�wAp�A�hA�A�AVAM�A�AXAoA��A��AbNA �AO�AoAAĜA��AjA�A�
AXA�+A�#AƨAhsAA�\AƨA�AdZA?}A��A�9A�A��A��A�An�AffA9XA�A��A�^A��A�/Az�AI�AJA�;A��AhsA�HA�A^5A1'A-A$�A��A�`A�DAv�AI�A(�A��A�mA��A`BA33A
ĜA
�A
bNA
A�A
 �A
A	��A	�A	�A	ƨA	p�A	33A�A�AA�^A�-A�A�`AA�AhsA�/A5?AƨA�AC�A��A��A��AI�A��AA ��A ��A j@���@�dZ@�33@��!@�J@���@�?}@���@�Z@��w@�dZ@���@�E�@��@���@�V@��@��@�1'@��;@���@�|�@�;d@���@�=q@��-@���@��H@���@�%@�@��@�@��@���@@�t�@�S�@�"�@��H@�~�@�O�@� �@�|�@�+@�?}@��@�Q�@�1'@���@��
@�"�@�`B@�9X@�w@�o@�5?@��`@��u@�  @ߕ�@�
=@ޟ�@�ff@���@�@�G�@ܓu@� �@ۅ@�v�@�-@�-@�@�7L@��@�%@ش9@�r�@ו�@�C�@�hs@Դ9@�I�@ӥ�@���@�V@��T@�`B@���@�z�@�9X@�b@ϕ�@�@�p�@�V@�9X@˅@��@ʧ�@�E�@�O�@�Q�@�|�@�33@�"�@�o@��@Ə\@�V@�{@ź^@Ł@�O�@ě�@�A�@�|�@���@���@¸R@�@+@�~�@�^5@�{@��`@�Z@�1@��;@��w@���@�+@��@���@���@��7@��@��D@�I�@�(�@���@���@���@�l�@�C�@��H@�{@�?}@�z�@�  @���@�"�@���@�J@��^@��@�O�@��@��j@�z�@�A�@�\)@�ff@�J@���@��-@�p�@��@�z�@�Q�@� �@��
@���@�C�@�E�@��-@�`B@�?}@��@���@���@���@���@�V@��@�@���@�X@�Ĝ@�A�@� �@��
@��@�;d@��@�V@��^@��`@��D@�I�@��;@�l�@��H@�5?@��T@�7L@��@�bN@�A�@��F@���@��\@��+@�~�@�~�@�v�@�~�@�v�@�J@��h@�V@��9@�j@�9X@��F@��R@�@�@�hs@��j@�(�@��;@��P@�dZ@��@��R@�~�@�^5@�-@��@�@�O�@�V@��9@��D@�z�@�j@�Z@�I�@��@���@��m@�ƨ@�K�@�"�@�o@��y@���@�^5@�M�@�$�@���@�p�@��j@��D@�A�@�b@��m@�ƨ@���@�K�@�@��y@���@�v�@�n�@�5?@�$�@�@���@��@�X@��`@�z�@��m@�K�@��H@���@�n�@�-@��T@��7@�X@��`@���@��@��F@��@�l�@�33@��+@�E�@�-@�@���@��h@�hs@�G�@��`@��m@�t�@�+@��R@�n�@��@�hs@�?}@���@��@���@��@�@�P@�P@~ff@}�T@}�h@|�j@|(�@{��@{33@z�H@z=q@yx�@y�@xbN@xb@w��@v��@u�T@u�h@uV@t��@tz�@tj@tZ@tI�@s�
@st�@s"�@r�@r��@r��@rM�@q�@qx�@q&�@p��@p�9@pr�@pQ�@pA�@p �@o�w@oK�@o
=@n��@n��@n�@n��@n5?@m�@m�h@m�@l�D@k�
@kC�@j�!@j~�@jn�@j=q@jJ@i��@iX@hĜ@hA�@g�w@g\)@f��@fV@e��@e`B@e�@d�/@d�@d9X@c��@c�@cC�@c"�@co@b�@b��@b��@a�^@`r�@` �@_�@_��@_
=@^�+@^V@^5?@^{@]�@]�h@]`B@]/@\�/@\�D@\I�@[�m@[t�@[33@["�@Z�@Z�H@Z�!@Z^5@Y��@Y�^@Y�7@Y�7@Yx�@Yhs@YX@Y7L@X��@X�@W�@W;d@V��@Vȴ@V�R@V�+@VE�@U�T@Up�@UO�@U/@UV@T�j@Sƨ@S�@St�@SdZ@S33@Rn�@Q��@Q��@Q��@Q��@QG�@P�`@Pr�@Pb@O�w@O|�@Ol�@N��@Nff@N{@M��@M@M?}@Lz�@L1@Kt�@Ko@J�!@J�\@JM�@J�@JJ@I�#@Ix�@Ihs@IG�@H�9@Hb@G�w@G|�@G\)@G;d@Fȴ@F$�@E�h@E`B@D�D@C"�@B�@B�\@B^5@BM�@B=q@B-@BJ@A��@Ahs@@��@@�@@�@@�@@bN@@A�@@b@?��@?�@>�+@=�T@<�@<�@<j@<Z@<(�@;t�@;33@;"�@;o@:�\@:-@9�@9X@9�@8�`@8�9@8�@8Q�@8A�@81'@7�w@6�y@6��@6v�@6ff@6E�@6$�@5�-@5�@5V@4��@4I�@3�
@3t�@3S�@3C�@333@3"�@3o@3@2�H@2�\@2M�@1��@1X@17L@1&�@1�@1%@1%@0��@0Ĝ@0��@0�u@0 �@/�;@/��@/;d@.�y@.�R@.V@.@-�-@-`B@-?}@,��@,�/@,�j@,�D@,I�@+��@+o@*��@*��@*~�@)��@)hs@)&�@(��@(Ĝ@(bN@'��@'|�@'
=@&ȴ@&��@&$�@&@%�T@%�-@%`B@%�@$�@$9X@#dZ@#@"��@"~�@"�@!��@!G�@!&�@!&�@ ��@ �9@ Q�@�@|�@�@ȴ@��@��@v�@E�@@�-@p�@�@��@�@�@�/@Z@�
@dZ@C�@33@�@��@��@~�@-@��@��@�@��@�9@bN@ �@�P@+@
=@
=@��@��@��@v�@ff@ff@ff@V@$�@�@�T@��@�-@�h@`B@/@�@��@�j@��@z�@z�@I�@9X@1@�m@�F@��@S�@@�H@��@�\@=q@�@�^@x�@hs@hs@X@G�@&�@%@%@��@��@�@A�@ �@b@�;@��@�@|�@\)@l�@\)@;d@+@
=@
=@�@�@�@�@ȴ@�R@�R@��@v�@{@@�T@��@�@p�@`B@O�@?}@�@��@��@��@�@z�@I�@9X@�@1@�
@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�%B
�JB
�\B
�hB
�{B
��B
��B
��B
��B
�?B
�^B
ÖB
��B
�/B
�yB
�B
�B
�B
�B
�B
�B
�ZB
�TB
��B
�uB&�B>wB~�BQ�BhsBL�B
�RB
�?B
�VB
dZB
��B
r�B
uB
2-B
�B	�B	�TB	�7B	O�B	��B	��B	u�B	[#B	��B	��B	�DB	T�B	'�B	�B	{B	�B	?}B	B�B	=qB	aHB	z�B	�7B	�hB	��B	�oB	�hB	��B	�-B	��B	�dB	�BB	��B
�B
bB
JB
A�B
H�B
>wB
9XB
>wB
B�B
;dB
=qB
S�B
aHB
N�B
6FB
A�B
2-B
1'B
>wB
;dB
8RB
1'B
1'B
 �B
33B
=qB
9XB
8RB
L�B
\)B
hsB
u�B
u�B
y�B
y�B
{�B
�B
�%B
�B
�B
}�B
�%B
�B
�B
�+B
~�B
q�B
n�B
n�B
r�B
w�B
z�B
}�B
� B
|�B
x�B
u�B
v�B
|�B
y�B
x�B
x�B
t�B
r�B
l�B
ffB
cTB
iyB
k�B
hsB
hsB
aHB
jB
jB
l�B
jB
jB
k�B
n�B
u�B
u�B
u�B
v�B
t�B
u�B
u�B
u�B
r�B
jB
l�B
o�B
o�B
n�B
m�B
hsB
cTB
gmB
jB
hsB
iyB
ffB
_;B
[#B
^5B
cTB
bNB
cTB
cTB
dZB
aHB
`BB
aHB
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
^5B
[#B
W
B
VB
P�B
P�B
XB
ZB
XB
S�B
F�B
B�B
L�B
J�B
E�B
D�B
H�B
L�B
K�B
J�B
J�B
H�B
E�B
B�B
=qB
E�B
E�B
C�B
@�B
B�B
D�B
A�B
?}B
A�B
@�B
@�B
?}B
>wB
@�B
?}B
A�B
C�B
A�B
?}B
A�B
C�B
B�B
B�B
B�B
B�B
?}B
=qB
8RB
8RB
49B
.B
33B
7LB
;dB
=qB
=qB
;dB
8RB
9XB
:^B
9XB
8RB
5?B
33B
,B
(�B
.B
,B
)�B
1'B
33B
49B
1'B
.B
(�B
 �B
$�B
+B
&�B
&�B
#�B
+B
)�B
+B
)�B
)�B
+B
(�B
)�B
%�B
#�B
%�B
$�B
#�B
'�B
,B
(�B
&�B
+B
)�B
&�B
$�B
�B
�B
�B
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
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
#�B
"�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
#�B
"�B
 �B
 �B
 �B
�B
�B
"�B
%�B
#�B
#�B
"�B
"�B
%�B
%�B
#�B
#�B
!�B
�B
!�B
%�B
&�B
&�B
$�B
"�B
�B
�B
$�B
&�B
%�B
&�B
%�B
#�B
#�B
'�B
%�B
%�B
%�B
%�B
 �B
"�B
 �B
%�B
%�B
$�B
#�B
#�B
#�B
&�B
%�B
&�B
)�B
+B
'�B
&�B
+B
0!B
0!B
0!B
0!B
0!B
/B
,B
+B
+B
-B
.B
-B
)�B
&�B
(�B
.B
-B
,B
-B
1'B
1'B
2-B
1'B
2-B
33B
49B
49B
49B
2-B
1'B
49B
49B
6FB
7LB
7LB
7LB
7LB
6FB
6FB
6FB
5?B
49B
7LB
8RB
7LB
6FB
6FB
8RB
7LB
5?B
5?B
33B
8RB
8RB
9XB
:^B
:^B
:^B
8RB
9XB
;dB
;dB
;dB
=qB
;dB
<jB
<jB
;dB
9XB
;dB
8RB
9XB
7LB
9XB
;dB
=qB
>wB
=qB
=qB
<jB
>wB
<jB
=qB
<jB
>wB
A�B
A�B
@�B
=qB
@�B
D�B
C�B
C�B
C�B
C�B
C�B
@�B
>wB
A�B
D�B
C�B
D�B
C�B
D�B
I�B
I�B
K�B
K�B
H�B
G�B
I�B
K�B
H�B
I�B
K�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
L�B
J�B
L�B
K�B
J�B
K�B
N�B
N�B
O�B
Q�B
Q�B
Q�B
Q�B
O�B
P�B
P�B
R�B
Q�B
Q�B
P�B
P�B
Q�B
R�B
S�B
S�B
S�B
T�B
T�B
S�B
R�B
R�B
T�B
VB
VB
T�B
S�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
XB
XB
XB
W
B
W
B
VB
T�B
VB
W
B
W
B
XB
W
B
XB
YB
[#B
[#B
[#B
ZB
ZB
\)B
\)B
]/B
]/B
\)B
[#B
ZB
W
B
W
B
]/B
]/B
]/B
[#B
]/B
_;B
_;B
`BB
_;B
_;B
_;B
`BB
_;B
_;B
`BB
_;B
`BB
aHB
bNB
aHB
bNB
aHB
`BB
aHB
bNB
cTB
dZB
cTB
cTB
cTB
bNB
aHB
aHB
`BB
cTB
dZB
e`B
e`B
dZB
dZB
dZB
dZB
ffB
ffB
e`B
dZB
cTB
ffB
gmB
gmB
ffB
dZB
e`B
iyB
iyB
hsB
gmB
ffB
gmB
gmB
hsB
hsB
iyB
hsB
gmB
iyB
iyB
iyB
hsB
ffB
iyB
hsB
k�B
k�B
l�B
l�B
l�B
m�B
l�B
l�B
m�B
l�B
jB
jB
l�B
m�B
n�B
m�B
k�B
k�B
l�B
m�B
k�B
hsB
p�B
p�B
q�B
r�B
r�B
r�B
q�B
p�B
p�B
p�B
q�B
s�B
s�B
r�B
q�B
p�B
o�B
o�B
n�B
o�B
o�B
r�B
s�B
s�B
r�B
q�B
t�B
t�B
t�B
r�B
r�B
t�B
r�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
t�B
s�B
w�B
x�B
x�B
x�B
w�B
v�B
u�B
x�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
{�B
z�B
z�B
y�B
y�B
x�B
z�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
z�B
y�B
{�B
z�B
z�B
{�B
|�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
~�B
}�B
|�B
{�B
|�B
}�B
~�B
~�B
}�B
}�B
� B
� B
� B
~�B
~�B
� B
~�B
�B
�B
� B
�B
�B
�B
�B
�B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�%B
�B
�B
�%B
�1B
�1B
�+B
�1B
�1B
�1B
�+B
�+B
�1B
�+B
�7B
�7B
�7B
�7B
�1B
�=B
�JB
�JB
�JB
�DB
�PB
�JB
�PB
�PB
�PB
�JB
�JB
�JB
�PB
�PB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�\B
�\B
�\B
�VB
�VB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�bB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�uB
��B
��B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�%B
�JB
�\B
�hB
�{B
��B
��B
��B
��B
�?B
�^B
ÖB
��B
�/B
�yB
�B
�B
�B
�B
�B
�B
�ZB
�TB
��B
�uB&�B>wB~�BQ�BhsBL�B
�RB
�?B
�VB
dZB
��B
r�B
uB
2-B
�B	�B	�TB	�7B	O�B	��B	��B	u�B	[#B	��B	��B	�DB	T�B	'�B	�B	{B	�B	?}B	B�B	=qB	aHB	z�B	�7B	�hB	��B	�oB	�hB	��B	�-B	��B	�dB	�BB	��B
�B
bB
JB
A�B
H�B
>wB
9XB
>wB
B�B
;dB
=qB
S�B
aHB
N�B
6FB
A�B
2-B
1'B
>wB
;dB
8RB
1'B
1'B
 �B
33B
=qB
9XB
8RB
L�B
\)B
hsB
u�B
u�B
y�B
y�B
{�B
�B
�%B
�B
�B
}�B
�%B
�B
�B
�+B
~�B
q�B
n�B
n�B
r�B
w�B
z�B
}�B
� B
|�B
x�B
u�B
v�B
|�B
y�B
x�B
x�B
t�B
r�B
l�B
ffB
cTB
iyB
k�B
hsB
hsB
aHB
jB
jB
l�B
jB
jB
k�B
n�B
u�B
u�B
u�B
v�B
t�B
u�B
u�B
u�B
r�B
jB
l�B
o�B
o�B
n�B
m�B
hsB
cTB
gmB
jB
hsB
iyB
ffB
_;B
[#B
^5B
cTB
bNB
cTB
cTB
dZB
aHB
`BB
aHB
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
^5B
[#B
W
B
VB
P�B
P�B
XB
ZB
XB
S�B
F�B
B�B
L�B
J�B
E�B
D�B
H�B
L�B
K�B
J�B
J�B
H�B
E�B
B�B
=qB
E�B
E�B
C�B
@�B
B�B
D�B
A�B
?}B
A�B
@�B
@�B
?}B
>wB
@�B
?}B
A�B
C�B
A�B
?}B
A�B
C�B
B�B
B�B
B�B
B�B
?}B
=qB
8RB
8RB
49B
.B
33B
7LB
;dB
=qB
=qB
;dB
8RB
9XB
:^B
9XB
8RB
5?B
33B
,B
(�B
.B
,B
)�B
1'B
33B
49B
1'B
.B
(�B
 �B
$�B
+B
&�B
&�B
#�B
+B
)�B
+B
)�B
)�B
+B
(�B
)�B
%�B
#�B
%�B
$�B
#�B
'�B
,B
(�B
&�B
+B
)�B
&�B
$�B
�B
�B
�B
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
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
#�B
"�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
#�B
"�B
 �B
 �B
 �B
�B
�B
"�B
%�B
#�B
#�B
"�B
"�B
%�B
%�B
#�B
#�B
!�B
�B
!�B
%�B
&�B
&�B
$�B
"�B
�B
�B
$�B
&�B
%�B
&�B
%�B
#�B
#�B
'�B
%�B
%�B
%�B
%�B
 �B
"�B
 �B
%�B
%�B
$�B
#�B
#�B
#�B
&�B
%�B
&�B
)�B
+B
'�B
&�B
+B
0!B
0!B
0!B
0!B
0!B
/B
,B
+B
+B
-B
.B
-B
)�B
&�B
(�B
.B
-B
,B
-B
1'B
1'B
2-B
1'B
2-B
33B
49B
49B
49B
2-B
1'B
49B
49B
6FB
7LB
7LB
7LB
7LB
6FB
6FB
6FB
5?B
49B
7LB
8RB
7LB
6FB
6FB
8RB
7LB
5?B
5?B
33B
8RB
8RB
9XB
:^B
:^B
:^B
8RB
9XB
;dB
;dB
;dB
=qB
;dB
<jB
<jB
;dB
9XB
;dB
8RB
9XB
7LB
9XB
;dB
=qB
>wB
=qB
=qB
<jB
>wB
<jB
=qB
<jB
>wB
A�B
A�B
@�B
=qB
@�B
D�B
C�B
C�B
C�B
C�B
C�B
@�B
>wB
A�B
D�B
C�B
D�B
C�B
D�B
I�B
I�B
K�B
K�B
H�B
G�B
I�B
K�B
H�B
I�B
K�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
L�B
J�B
L�B
K�B
J�B
K�B
N�B
N�B
O�B
Q�B
Q�B
Q�B
Q�B
O�B
P�B
P�B
R�B
Q�B
Q�B
P�B
P�B
Q�B
R�B
S�B
S�B
S�B
T�B
T�B
S�B
R�B
R�B
T�B
VB
VB
T�B
S�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
XB
XB
XB
W
B
W
B
VB
T�B
VB
W
B
W
B
XB
W
B
XB
YB
[#B
[#B
[#B
ZB
ZB
\)B
\)B
]/B
]/B
\)B
[#B
ZB
W
B
W
B
]/B
]/B
]/B
[#B
]/B
_;B
_;B
`BB
_;B
_;B
_;B
`BB
_;B
_;B
`BB
_;B
`BB
aHB
bNB
aHB
bNB
aHB
`BB
aHB
bNB
cTB
dZB
cTB
cTB
cTB
bNB
aHB
aHB
`BB
cTB
dZB
e`B
e`B
dZB
dZB
dZB
dZB
ffB
ffB
e`B
dZB
cTB
ffB
gmB
gmB
ffB
dZB
e`B
iyB
iyB
hsB
gmB
ffB
gmB
gmB
hsB
hsB
iyB
hsB
gmB
iyB
iyB
iyB
hsB
ffB
iyB
hsB
k�B
k�B
l�B
l�B
l�B
m�B
l�B
l�B
m�B
l�B
jB
jB
l�B
m�B
n�B
m�B
k�B
k�B
l�B
m�B
k�B
hsB
p�B
p�B
q�B
r�B
r�B
r�B
q�B
p�B
p�B
p�B
q�B
s�B
s�B
r�B
q�B
p�B
o�B
o�B
n�B
o�B
o�B
r�B
s�B
s�B
r�B
q�B
t�B
t�B
t�B
r�B
r�B
t�B
r�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
t�B
s�B
w�B
x�B
x�B
x�B
w�B
v�B
u�B
x�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
{�B
z�B
z�B
y�B
y�B
x�B
z�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
z�B
y�B
{�B
z�B
z�B
{�B
|�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
~�B
}�B
|�B
{�B
|�B
}�B
~�B
~�B
}�B
}�B
� B
� B
� B
~�B
~�B
� B
~�B
�B
�B
� B
�B
�B
�B
�B
�B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�%B
�B
�B
�%B
�1B
�1B
�+B
�1B
�1B
�1B
�+B
�+B
�1B
�+B
�7B
�7B
�7B
�7B
�1B
�=B
�JB
�JB
�JB
�DB
�PB
�JB
�PB
�PB
�PB
�JB
�JB
�JB
�PB
�PB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�\B
�\B
�\B
�VB
�VB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�bB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�uB
��B
��B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220715090106                              AO  ARCAADJP                                                                    20220715090106    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220715090106  QCP$                G�O�G�O�G�O�5F83E           AO  ARGQQCPL                                                                    20220715090106  QCF$                G�O�G�O�G�O�0               