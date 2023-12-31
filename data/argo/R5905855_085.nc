CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:25:28Z creation;2022-06-04T19:25:29Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604192528  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               UA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�{�[f�1   @�{	���@*��`A�7�d�\)1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B���B�ffB�  B�33BǙ�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C�C�C�C�fC�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4ffC5ffC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP� CQ��CS�fCV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�<�DȀ D��3D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�z�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B�8RB���B���B���B���B���B���B���B���B���B���B�k�B�8RB���B�B�k�B˞�B���B���B���B���B���B���B���B���B���B�B���B���B���C��C��C��C��C	��C��C�C�C�C�\C�\C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C4O\C5O\C7�\C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CN�CPh�CQ��CS�\CU��CW�\CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Du��Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�@RD�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�9�D�}D��RD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`vA�aA�b�A�a�A�`�A�bA�d�A�d�A�\�A�Y�A�X�A�T�A�U2A�_�A�[WA�S&A�($A�_A��GA��]A��yAҰ!AҒ�A�v�A�^�A�:^A�'A���A��}A��A��Aѣ�Aд�A�e�A�v`A�{JA��2A�hsA��3A�՛AǶA��8A�+A�xlA�C-A�FA�� A��QA��vAþBA�>�A��A��;A�6FA��%A��@A�ÖA��A�"A��A���A�s�A�<6A��dA�A A���A�~]A�I�A��A��0A�g�A�� A�^5A�
�A��A�-�A�ѷA���A��A�c�A�A��mA�B[A�A��~A�\�A�� A��A��ZA��A�&�Aw��An&�AgXyAa��A]7�AZ�AXg�AU>�ASB�AOxlAL�~AIɆAI>BAG��AA�A>	lA<�+A9�A6�*A5�A3S�A2o�A2!-A1�]A1��A1��A1w�A1@A0$tA.�A. �A-�'A-��A-aA,��A+�LA+@�A*�6A*�^A*��A*��A)��A)�)A)�XA)o A(�XA'v`A&��A&�A$ԕA"�A!��A�NA�xAI�A�A�hA��AuA0�A��A�A��A�A�2A�]A�Aj�A iA��A�A�XA�A�DA�A��A!�A�:AU2A
=A� A�>A�AS&A2aA�A��A \A��A�AA=�A�A��A�UA�	A�yAP�AA��AxlAW?A�A��Ac A?A��A~�A<�A��AA A9XA�A��A��AoiAc AVmA%FA�A�=AH�A�xAPHA �A�6AbNA�)A�*A�VA��AiDAH�A
��A
�A	��A	Z�A	  A�_A($A�A�.A�A��AH�A'�Aj�A%�Av`AS&A?�A6�A��A~�A#�A��A�\A ��A 	l@�zx@�hs@�U2@���@��@���@��)@��9@�&�@�n/@��R@���@�e�@��@���@�+@��@�@��@�5�@�@�X@�%@��p@�n�@�e@���@���@���@밊@��@�-w@��@�;�@�H@��@淀@�PH@�F�@�ϫ@��@�{�@ᙚ@�IR@���@�!�@��@�=@�!�@���@��@�@�Q�@�N<@�I�@�%�@�rG@��	@֕@�*�@��A@�@O@��"@��@�q@�K^@��@���@Ӳ-@�a@���@�� @�(@��@иR@НI@��@�g8@�?@�@�@@Γu@·�@�M@��a@�X@̹$@�
�@ˋ�@�W?@��@�^5@���@��,@ȑ�@�ݘ@���@Ž�@�R�@Ë�@�:�@�(�@��@��@¾@���@��@���@�N<@��@�h�@�)�@�~@���@��@�[W@�N<@�P�@�G�@�@�'R@��^@�j�@��@���@�Ta@�$@��@��@��:@�0�@��B@�N�@��V@�9�@��@���@�q�@�1�@���@��@���@���@��@���@�Y@���@�4n@��D@��j@��H@���@�+@��@�h
@�>B@�'R@�M@�u@��j@��@�iD@��@�
�@���@���@���@�L0@��@�A�@�@�Z�@�|�@��f@��@�ی@��,@���@�Xy@�8�@�~@��@���@��9@�F@���@�\�@��@��=@�+�@��|@��s@�u�@�N�@�M@��w@��"@�\�@��@���@�M@��g@�(�@�ی@��\@�h�@��@��@�x�@�.I@��@��3@�@@���@�Z@��@�ϫ@�s�@���@�Ov@�)�@���@�c@�0�@���@��4@�M@��K@��k@�dZ@���@���@�V�@�	@��>@��Q@��'@�{J@�c�@�RT@�:�@�$t@�S@��@��@�q�@�I�@�b@��@�J�@�#�@��P@���@�n�@�M@�u�@��9@�!�@��@��N@���@���@���@�K^@���@���@�\)@�;@��@�C-@�1�@��@�	@��@��.@��@���@�@��@�l�@��@��@���@��h@�M�@���@��@��k@�u�@�Dg@��@���@�u�@�e�@�8�@���@��3@���@�S�@��R@��A@�C�@�e@��@��0@�dZ@�2a@��@�
=@��@��K@�ߤ@���@���@�M�@�:�@�%�@�J@���@�|@�hs@��@��@���@��@�r�@�b@��0@���@�\)@�'�@�	l@��@���@�ff@�/�@�	@O@~�x@}�@}��@}IR@|�|@|bN@{�;@{e�@z��@z	@yIR@x��@xXy@x~@w��@wqv@v�s@v0U@u�d@t�@t,=@s{J@sY@r�m@ri�@r5?@q��@qzx@q@p�e@p,=@o�$@ot�@o=@o@nȴ@n��@nTa@m�@lA�@k�F@k"�@j�F@i�@hH@h�@g�4@f��@f�6@fV@f�@e�^@e��@e�@d�@cO@b�@b�<@b�@bl�@b.�@a|@`�@`!@_�@_x@_\)@^�@]\�@\�j@\7@[��@[l�@Zȴ@Y��@Y�@Ya�@Y�@X��@Xoi@X�@W_p@V��@V��@Vq�@VB[@V�@U��@U�h@U��@Uj@UIR@U�@T�f@T��@Tw�@TG@S�@@S�4@R�B@RC�@Q�D@Q��@Q�d@Q�z@Q�7@QO�@Q!�@P�	@P�/@P��@P2�@O�P@O8@N��@N͟@Nh
@M@Mw2@MF@L�@L�9@LD�@L�@K~�@J�]@JE�@I�@I�~@IV@H�@H�z@Hm�@H>B@HG@G��@G��@Gt�@GMj@Fں@F��@FW�@E��@Em]@D��@D7�@C�]@C�@C�@C�@C&@Bxl@A�#@A\�@A!�@@��@@�/@@�@@Ft@?�;@?�*@>�@>M�@>�@=��@=��@=IR@<ی@<~(@<!@;;d@:�L@:$�@9�>@9ԕ@9�@9��@9T�@98�@9%F@9�@8ѷ@8�_@8tT@8V�@7�m@7]�@7>�@7.I@7o@6ߤ@6�6@6E�@6	@5�@5��@5Vm@5:�@5+@4��@4�9@4��@4H@3�&@3�@3�	@3S@2��@2�A@2@1�o@1��@1�@1�z@1��@1�@0�9@/�A@/�V@/v`@/>�@/!-@.�@.#:@-�#@-m]@,��@,��@,��@,6@+�@+ݘ@+�f@+E9@+�@*�8@*�X@*��@)��@)��@)�@)G�@(�K@(�@'�a@'��@'dZ@'!-@'@&�]@&��@&W�@&GE@&C�@&-@%�^@%2a@$�|@$�$@#x@#9�@#�@"�2@"�@"��@"�@"Ta@"{@!��@!�M@!O�@!:�@ �U@ c�@ 'R@ G@��@4�@�s@�A@1�@�@��@�@�S@Y�@�@I�@�@�Q@��@�*@o�@>�@/�@Y@�8@�,@��@@�@�S@J�@�@�f@�p@�O@�_@r�@(�@1@� @�@o�@J#@
=@�c@�@�}@��@YK@5?@@��@��@��@�M@O�@&�@�5@�/@��@��@�@7�@�Q@S�@�@�@��@YK@H�@8�@!�@�@
�@@ �@��@��@|@5�@	l@�@�$@�4@�Y@Xy@?�@~@�@�m@��@�}@�@�@�0@��@v`@Z�@�@�@�"@��@�\@q�@Ta@u@��@��@�M@j@`B@*0@��@��@��@�@�4@��@q@"h@��@�W@ݘ@��@��@=@�@
�@
��@
��@
�@
��@
�6@
W�@
=q@
�@	�@	�@	�^@	�@	��@	�=@	��@	��@	o @	=�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`vA�aA�b�A�a�A�`�A�bA�d�A�d�A�\�A�Y�A�X�A�T�A�U2A�_�A�[WA�S&A�($A�_A��GA��]A��yAҰ!AҒ�A�v�A�^�A�:^A�'A���A��}A��A��Aѣ�Aд�A�e�A�v`A�{JA��2A�hsA��3A�՛AǶA��8A�+A�xlA�C-A�FA�� A��QA��vAþBA�>�A��A��;A�6FA��%A��@A�ÖA��A�"A��A���A�s�A�<6A��dA�A A���A�~]A�I�A��A��0A�g�A�� A�^5A�
�A��A�-�A�ѷA���A��A�c�A�A��mA�B[A�A��~A�\�A�� A��A��ZA��A�&�Aw��An&�AgXyAa��A]7�AZ�AXg�AU>�ASB�AOxlAL�~AIɆAI>BAG��AA�A>	lA<�+A9�A6�*A5�A3S�A2o�A2!-A1�]A1��A1��A1w�A1@A0$tA.�A. �A-�'A-��A-aA,��A+�LA+@�A*�6A*�^A*��A*��A)��A)�)A)�XA)o A(�XA'v`A&��A&�A$ԕA"�A!��A�NA�xAI�A�A�hA��AuA0�A��A�A��A�A�2A�]A�Aj�A iA��A�A�XA�A�DA�A��A!�A�:AU2A
=A� A�>A�AS&A2aA�A��A \A��A�AA=�A�A��A�UA�	A�yAP�AA��AxlAW?A�A��Ac A?A��A~�A<�A��AA A9XA�A��A��AoiAc AVmA%FA�A�=AH�A�xAPHA �A�6AbNA�)A�*A�VA��AiDAH�A
��A
�A	��A	Z�A	  A�_A($A�A�.A�A��AH�A'�Aj�A%�Av`AS&A?�A6�A��A~�A#�A��A�\A ��A 	l@�zx@�hs@�U2@���@��@���@��)@��9@�&�@�n/@��R@���@�e�@��@���@�+@��@�@��@�5�@�@�X@�%@��p@�n�@�e@���@���@���@밊@��@�-w@��@�;�@�H@��@淀@�PH@�F�@�ϫ@��@�{�@ᙚ@�IR@���@�!�@��@�=@�!�@���@��@�@�Q�@�N<@�I�@�%�@�rG@��	@֕@�*�@��A@�@O@��"@��@�q@�K^@��@���@Ӳ-@�a@���@�� @�(@��@иR@НI@��@�g8@�?@�@�@@Γu@·�@�M@��a@�X@̹$@�
�@ˋ�@�W?@��@�^5@���@��,@ȑ�@�ݘ@���@Ž�@�R�@Ë�@�:�@�(�@��@��@¾@���@��@���@�N<@��@�h�@�)�@�~@���@��@�[W@�N<@�P�@�G�@�@�'R@��^@�j�@��@���@�Ta@�$@��@��@��:@�0�@��B@�N�@��V@�9�@��@���@�q�@�1�@���@��@���@���@��@���@�Y@���@�4n@��D@��j@��H@���@�+@��@�h
@�>B@�'R@�M@�u@��j@��@�iD@��@�
�@���@���@���@�L0@��@�A�@�@�Z�@�|�@��f@��@�ی@��,@���@�Xy@�8�@�~@��@���@��9@�F@���@�\�@��@��=@�+�@��|@��s@�u�@�N�@�M@��w@��"@�\�@��@���@�M@��g@�(�@�ی@��\@�h�@��@��@�x�@�.I@��@��3@�@@���@�Z@��@�ϫ@�s�@���@�Ov@�)�@���@�c@�0�@���@��4@�M@��K@��k@�dZ@���@���@�V�@�	@��>@��Q@��'@�{J@�c�@�RT@�:�@�$t@�S@��@��@�q�@�I�@�b@��@�J�@�#�@��P@���@�n�@�M@�u�@��9@�!�@��@��N@���@���@���@�K^@���@���@�\)@�;@��@�C-@�1�@��@�	@��@��.@��@���@�@��@�l�@��@��@���@��h@�M�@���@��@��k@�u�@�Dg@��@���@�u�@�e�@�8�@���@��3@���@�S�@��R@��A@�C�@�e@��@��0@�dZ@�2a@��@�
=@��@��K@�ߤ@���@���@�M�@�:�@�%�@�J@���@�|@�hs@��@��@���@��@�r�@�b@��0@���@�\)@�'�@�	l@��@���@�ff@�/�@�	@O@~�x@}�@}��@}IR@|�|@|bN@{�;@{e�@z��@z	@yIR@x��@xXy@x~@w��@wqv@v�s@v0U@u�d@t�@t,=@s{J@sY@r�m@ri�@r5?@q��@qzx@q@p�e@p,=@o�$@ot�@o=@o@nȴ@n��@nTa@m�@lA�@k�F@k"�@j�F@i�@hH@h�@g�4@f��@f�6@fV@f�@e�^@e��@e�@d�@cO@b�@b�<@b�@bl�@b.�@a|@`�@`!@_�@_x@_\)@^�@]\�@\�j@\7@[��@[l�@Zȴ@Y��@Y�@Ya�@Y�@X��@Xoi@X�@W_p@V��@V��@Vq�@VB[@V�@U��@U�h@U��@Uj@UIR@U�@T�f@T��@Tw�@TG@S�@@S�4@R�B@RC�@Q�D@Q��@Q�d@Q�z@Q�7@QO�@Q!�@P�	@P�/@P��@P2�@O�P@O8@N��@N͟@Nh
@M@Mw2@MF@L�@L�9@LD�@L�@K~�@J�]@JE�@I�@I�~@IV@H�@H�z@Hm�@H>B@HG@G��@G��@Gt�@GMj@Fں@F��@FW�@E��@Em]@D��@D7�@C�]@C�@C�@C�@C&@Bxl@A�#@A\�@A!�@@��@@�/@@�@@Ft@?�;@?�*@>�@>M�@>�@=��@=��@=IR@<ی@<~(@<!@;;d@:�L@:$�@9�>@9ԕ@9�@9��@9T�@98�@9%F@9�@8ѷ@8�_@8tT@8V�@7�m@7]�@7>�@7.I@7o@6ߤ@6�6@6E�@6	@5�@5��@5Vm@5:�@5+@4��@4�9@4��@4H@3�&@3�@3�	@3S@2��@2�A@2@1�o@1��@1�@1�z@1��@1�@0�9@/�A@/�V@/v`@/>�@/!-@.�@.#:@-�#@-m]@,��@,��@,��@,6@+�@+ݘ@+�f@+E9@+�@*�8@*�X@*��@)��@)��@)�@)G�@(�K@(�@'�a@'��@'dZ@'!-@'@&�]@&��@&W�@&GE@&C�@&-@%�^@%2a@$�|@$�$@#x@#9�@#�@"�2@"�@"��@"�@"Ta@"{@!��@!�M@!O�@!:�@ �U@ c�@ 'R@ G@��@4�@�s@�A@1�@�@��@�@�S@Y�@�@I�@�@�Q@��@�*@o�@>�@/�@Y@�8@�,@��@@�@�S@J�@�@�f@�p@�O@�_@r�@(�@1@� @�@o�@J#@
=@�c@�@�}@��@YK@5?@@��@��@��@�M@O�@&�@�5@�/@��@��@�@7�@�Q@S�@�@�@��@YK@H�@8�@!�@�@
�@@ �@��@��@|@5�@	l@�@�$@�4@�Y@Xy@?�@~@�@�m@��@�}@�@�@�0@��@v`@Z�@�@�@�"@��@�\@q�@Ta@u@��@��@�M@j@`B@*0@��@��@��@�@�4@��@q@"h@��@�W@ݘ@��@��@=@�@
�@
��@
��@
�@
��@
�6@
W�@
=q@
�@	�@	�@	�^@	�@	��@	�=@	��@	��@	o @	=�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
X�B
X�B
XyB
X_B
X�B
W�B
W�B
W�B
V�B
VmB
VSB
V9B
VSB
VSB
VB
U�B
S�B
R�B
P}B
P.B
OBB
N�B
J�B
EmB
A;B
:�B
6�B
.B
&�B
$�B
$B
~B
AB	�xB
B
�B	��B	�B
%zB
+6B
8B
P.B
ZB
j�B
wfB
��B
�zB
�YB
�YB
��B
��B
�,B
��B
�`B�B/ BU�Ba�Bn�By�B��B��B��B��BɺB��B��B�dB�6B�B�}B�.B�<B��B��B�"B�B�xB�uB�%B�B��B�}Bh$BH�B�B
��B
֡B
��B
��B
d�B	�pB	��B	~�B	^�B	J#B	=�B	33B	�B	B	~B	�B	�B	 iB	zB	'RB	#:B	;B	OB	 B	&2B	3B	@�B	F�B	I7B	J�B	MPB	WsB	h�B	��B	�KB	�+B	�B	� B	ؓB	��B	�B	�B	��B	�B
�B
�B
#nB
-�B
3MB
8�B
1�B
/OB
-)B
2aB
'�B
eB
�B	�	B	�WB	��B	�B	��B	�hB	�zB
1�B
?.B
@ B
AUB
ESB
MPB
Q B
S�B
Q�B
O�B
OvB
NB
J	B
MB
N"B
K�B
J�B
I�B
MjB
L0B
J	B
E�B
H1B
GEB
GB
F�B
F�B
F�B
HB
H�B
IlB
J�B
LJB
MPB
M�B
MB
LdB
J=B
H�B
HfB
HKB
H�B
H1B
HfB
GzB
F�B
G�B
F�B
E�B
CGB
C�B
F�B
GB
GzB
HfB
IlB
IRB
I7B
IB
GzB
EmB
C�B
>�B
?�B
>BB
=�B
="B
=�B
>�B
?B
>�B
>(B
=qB
=�B
<B
;JB
;B
9�B
8�B
7�B
6zB
5%B
49B
49B
2aB
1[B
/OB
+kB
(�B
'�B
'mB
'B
&B
$tB
#B
!�B
 �B
�B
�B
�B
7B
�B
?B
SB
�B
�B
�B
B
B
hB
�B
.B
�B
�B
�B
�B
�B
DB
B
^B

�B

	B
	�B
	�B
	lB
	B
�B
B
�B
�B
�B
	B
�B
	B
1B
�B
�B
�B
EB
�B
SB
B
�B
GB
GB
uB
�B
oB
 �B	��B	�}B	��B	�.B	�.B	��B	�B	��B	�wB	�(B	�B	��B	�]B	�wB	��B	�wB	��B	�]B	�]B	�]B	�(B	�cB	�.B	��B	��B	��B	��B	��B	�]B	�BB	�}B	��B	�wB	��B	�wB	��B	��B	��B	��B	��B	��B	�.B	�wB	��B	�B	��B	��B	��B	��B	��B	�wB	�BB	�BB	��B	��B	��B	��B	��B	��B	�cB	��B	�wB	�]B	�BB	�]B	�BB	��B	��B	��B
 4B
  B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 OB
 �B
 �B
�B
B
[B
�B
�B
�B
-B
B
�B
MB
gB
B
B
�B
B
B
B
B
B
�B
_B
�B
�B
KB
fB
fB
�B
	RB
	�B

#B

XB

XB

�B
xB
0B
�B
�B
�B
B
�B
(B
B
(B
�B
(B
�B
�B
�B
�B
�B
�B
HB
�B
 B
4B
�B
�B
�B
�B
 B
B
TB
�B
�B
�B
�B
�B
�B
uB
,B
aB
�B
�B
�B
B
�B
�B
2B
�B
B
B
mB
�B
�B
�B
�B
�B
�B
�B
_B
yB
�B
�B
KB
�B
B
7B
�B
	B
=B
B
�B
B
xB
�B
B
�B
/B
/B
dB
~B
�B
�B
�B
�B
jB
!B
B
!B
�B
�B
 B
 �B
!�B
"NB
"�B
"�B
"�B
"�B
#TB
$&B
$&B
$@B
$�B
$�B
%`B
%�B
%�B
%�B
%�B
%�B
%�B
&2B
&B
&�B
'B
'RB
'�B
(
B
(
B
'�B
(�B
(�B
)B
)DB
)DB
)yB
*B
*B
*�B
*�B
*�B
+6B
+kB
+�B
+�B
,�B
,�B
-]B
-�B
-�B
-�B
.}B
.�B
.�B
/ B
/ B
/B
/B
/B
/�B
/�B
/�B
/�B
0B
0;B
0�B
0oB
1B
1'B
1'B
1'B
1AB
1�B
1�B
1�B
2-B
2-B
2-B
2GB
2GB
2�B
2�B
2aB
2�B
33B
3�B
3�B
3�B
3�B
4B
4TB
4�B
5%B
5tB
5�B
6�B
6�B
6�B
72B
7fB
8B
88B
88B
8�B
8�B
9�B
:B
9�B
:xB
:�B
:�B
;JB
;�B
<�B
<�B
=<B
=<B
=�B
=�B
=�B
>(B
?HB
?�B
?.B
@4B
A�B
A�B
A�B
AUB
A;B
BAB
B�B
CB
C-B
C-B
C�B
E�B
FYB
E�B
E9B
EmB
FYB
GB
GB
GB
GEB
F�B
H1B
I7B
IlB
I7B
I�B
IRB
H�B
IB
IRB
I7B
I�B
J	B
J#B
J�B
KDB
K�B
K�B
L0B
LdB
L�B
M�B
M�B
NB
NVB
OBB
O�B
O�B
O�B
O�B
P.B
PHB
P}B
P}B
P�B
Q B
Q B
Q�B
Q�B
R:B
RTB
RTB
RTB
R�B
SB
S&B
S&B
S@B
S@B
S�B
TB
TaB
T�B
T�B
UB
U�B
UgB
UgB
UgB
U�B
U�B
U�B
V�B
W
B
W�B
W�B
WsB
W�B
X+B
X�B
Y1B
YeB
YB
YKB
Y1B
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
ZB
[WB
\CB
]~B
]�B
^B
^B
^B
]�B
]�B
^5B
^5B
_;B
_�B
_�B
_�B
`B
`'B
`\B
`BB
_�B
_;B
^�B
_pB
_�B
`BB
`�B
`�B
`�B
a-B
aHB
a�B
bNB
c:B
cTB
c�B
d&B
dZB
dZB
d�B
d�B
d�B
e,B
e�B
e�B
e�B
f2B
ffB
ffB
f�B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
h$B
h>B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i_B
iyB
jB
j0B
jKB
jKB
j0B
jeB
j0B
j0B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
jB
jeB
j0B
j0B
jKB
j�B
j�B
j�B
kB
k6B
l"B
l�B
l�B
mB
m)B
m)B
mCB
m]B
m�B
m�B
mwB
m]B
m�B
n/B
n/B
ncB
o�B
o�B
pB
p!B
p!B
p!B
pUB
p�B
p�B
p�B
qvB
qvB
q[B
r-B
r�B
r�B
r�B
shB
s�B
s�B
tB
tnB
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
v+B
vB
v+B
v`B
vzB
vzB
vzB
v�B
v�B
v�B
v�B
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
y>B
y>B
yXB
yrB
y�B
y�B
z*B
z*B
zDB
zxB
z�B
z�B
z�B
z�B
{B
{B
{B
{0B
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|B
|PB
|�B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~]B
~wB
~�B
~�B
B
B
B
.B
cB
�B
�B
�B
�B
�B
�4B
�OB
�OB
�OB
��B
��B
��B
��B
��B
��B
� B
�;B
�UB
�;B
�UB
��B
��B
��B
��B
�B
�[B
�uB
�uB
��B
��B
��B
��B
�-B
��B
��B
��B
��B
��B
�3B
��B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
�B
�%B
�?B
�?B
�YB
�YB
�?B
�?B
�YB
�%B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
X�B
X�B
XyB
X_B
X�B
W�B
W�B
W�B
V�B
VmB
VSB
V9B
VSB
VSB
VB
U�B
S�B
R�B
P}B
P.B
OBB
N�B
J�B
EmB
A;B
:�B
6�B
.B
&�B
$�B
$B
~B
AB	�xB
B
�B	��B	�B
%zB
+6B
8B
P.B
ZB
j�B
wfB
��B
�zB
�YB
�YB
��B
��B
�,B
��B
�`B�B/ BU�Ba�Bn�By�B��B��B��B��BɺB��B��B�dB�6B�B�}B�.B�<B��B��B�"B�B�xB�uB�%B�B��B�}Bh$BH�B�B
��B
֡B
��B
��B
d�B	�pB	��B	~�B	^�B	J#B	=�B	33B	�B	B	~B	�B	�B	 iB	zB	'RB	#:B	;B	OB	 B	&2B	3B	@�B	F�B	I7B	J�B	MPB	WsB	h�B	��B	�KB	�+B	�B	� B	ؓB	��B	�B	�B	��B	�B
�B
�B
#nB
-�B
3MB
8�B
1�B
/OB
-)B
2aB
'�B
eB
�B	�	B	�WB	��B	�B	��B	�hB	�zB
1�B
?.B
@ B
AUB
ESB
MPB
Q B
S�B
Q�B
O�B
OvB
NB
J	B
MB
N"B
K�B
J�B
I�B
MjB
L0B
J	B
E�B
H1B
GEB
GB
F�B
F�B
F�B
HB
H�B
IlB
J�B
LJB
MPB
M�B
MB
LdB
J=B
H�B
HfB
HKB
H�B
H1B
HfB
GzB
F�B
G�B
F�B
E�B
CGB
C�B
F�B
GB
GzB
HfB
IlB
IRB
I7B
IB
GzB
EmB
C�B
>�B
?�B
>BB
=�B
="B
=�B
>�B
?B
>�B
>(B
=qB
=�B
<B
;JB
;B
9�B
8�B
7�B
6zB
5%B
49B
49B
2aB
1[B
/OB
+kB
(�B
'�B
'mB
'B
&B
$tB
#B
!�B
 �B
�B
�B
�B
7B
�B
?B
SB
�B
�B
�B
B
B
hB
�B
.B
�B
�B
�B
�B
�B
DB
B
^B

�B

	B
	�B
	�B
	lB
	B
�B
B
�B
�B
�B
	B
�B
	B
1B
�B
�B
�B
EB
�B
SB
B
�B
GB
GB
uB
�B
oB
 �B	��B	�}B	��B	�.B	�.B	��B	�B	��B	�wB	�(B	�B	��B	�]B	�wB	��B	�wB	��B	�]B	�]B	�]B	�(B	�cB	�.B	��B	��B	��B	��B	��B	�]B	�BB	�}B	��B	�wB	��B	�wB	��B	��B	��B	��B	��B	��B	�.B	�wB	��B	�B	��B	��B	��B	��B	��B	�wB	�BB	�BB	��B	��B	��B	��B	��B	��B	�cB	��B	�wB	�]B	�BB	�]B	�BB	��B	��B	��B
 4B
  B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 OB
 �B
 �B
�B
B
[B
�B
�B
�B
-B
B
�B
MB
gB
B
B
�B
B
B
B
B
B
�B
_B
�B
�B
KB
fB
fB
�B
	RB
	�B

#B

XB

XB

�B
xB
0B
�B
�B
�B
B
�B
(B
B
(B
�B
(B
�B
�B
�B
�B
�B
�B
HB
�B
 B
4B
�B
�B
�B
�B
 B
B
TB
�B
�B
�B
�B
�B
�B
uB
,B
aB
�B
�B
�B
B
�B
�B
2B
�B
B
B
mB
�B
�B
�B
�B
�B
�B
�B
_B
yB
�B
�B
KB
�B
B
7B
�B
	B
=B
B
�B
B
xB
�B
B
�B
/B
/B
dB
~B
�B
�B
�B
�B
jB
!B
B
!B
�B
�B
 B
 �B
!�B
"NB
"�B
"�B
"�B
"�B
#TB
$&B
$&B
$@B
$�B
$�B
%`B
%�B
%�B
%�B
%�B
%�B
%�B
&2B
&B
&�B
'B
'RB
'�B
(
B
(
B
'�B
(�B
(�B
)B
)DB
)DB
)yB
*B
*B
*�B
*�B
*�B
+6B
+kB
+�B
+�B
,�B
,�B
-]B
-�B
-�B
-�B
.}B
.�B
.�B
/ B
/ B
/B
/B
/B
/�B
/�B
/�B
/�B
0B
0;B
0�B
0oB
1B
1'B
1'B
1'B
1AB
1�B
1�B
1�B
2-B
2-B
2-B
2GB
2GB
2�B
2�B
2aB
2�B
33B
3�B
3�B
3�B
3�B
4B
4TB
4�B
5%B
5tB
5�B
6�B
6�B
6�B
72B
7fB
8B
88B
88B
8�B
8�B
9�B
:B
9�B
:xB
:�B
:�B
;JB
;�B
<�B
<�B
=<B
=<B
=�B
=�B
=�B
>(B
?HB
?�B
?.B
@4B
A�B
A�B
A�B
AUB
A;B
BAB
B�B
CB
C-B
C-B
C�B
E�B
FYB
E�B
E9B
EmB
FYB
GB
GB
GB
GEB
F�B
H1B
I7B
IlB
I7B
I�B
IRB
H�B
IB
IRB
I7B
I�B
J	B
J#B
J�B
KDB
K�B
K�B
L0B
LdB
L�B
M�B
M�B
NB
NVB
OBB
O�B
O�B
O�B
O�B
P.B
PHB
P}B
P}B
P�B
Q B
Q B
Q�B
Q�B
R:B
RTB
RTB
RTB
R�B
SB
S&B
S&B
S@B
S@B
S�B
TB
TaB
T�B
T�B
UB
U�B
UgB
UgB
UgB
U�B
U�B
U�B
V�B
W
B
W�B
W�B
WsB
W�B
X+B
X�B
Y1B
YeB
YB
YKB
Y1B
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
ZB
[WB
\CB
]~B
]�B
^B
^B
^B
]�B
]�B
^5B
^5B
_;B
_�B
_�B
_�B
`B
`'B
`\B
`BB
_�B
_;B
^�B
_pB
_�B
`BB
`�B
`�B
`�B
a-B
aHB
a�B
bNB
c:B
cTB
c�B
d&B
dZB
dZB
d�B
d�B
d�B
e,B
e�B
e�B
e�B
f2B
ffB
ffB
f�B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
h$B
h>B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i_B
iyB
jB
j0B
jKB
jKB
j0B
jeB
j0B
j0B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
jB
jeB
j0B
j0B
jKB
j�B
j�B
j�B
kB
k6B
l"B
l�B
l�B
mB
m)B
m)B
mCB
m]B
m�B
m�B
mwB
m]B
m�B
n/B
n/B
ncB
o�B
o�B
pB
p!B
p!B
p!B
pUB
p�B
p�B
p�B
qvB
qvB
q[B
r-B
r�B
r�B
r�B
shB
s�B
s�B
tB
tnB
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
v+B
vB
v+B
v`B
vzB
vzB
vzB
v�B
v�B
v�B
v�B
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
y>B
y>B
yXB
yrB
y�B
y�B
z*B
z*B
zDB
zxB
z�B
z�B
z�B
z�B
{B
{B
{B
{0B
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|B
|PB
|�B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~]B
~wB
~�B
~�B
B
B
B
.B
cB
�B
�B
�B
�B
�B
�4B
�OB
�OB
�OB
��B
��B
��B
��B
��B
��B
� B
�;B
�UB
�;B
�UB
��B
��B
��B
��B
�B
�[B
�uB
�uB
��B
��B
��B
��B
�-B
��B
��B
��B
��B
��B
�3B
��B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
�B
�%B
�?B
�?B
�YB
�YB
�?B
�?B
�YB
�%B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105245  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192529  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192529                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042537  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042537  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                