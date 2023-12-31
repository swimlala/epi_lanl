CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-30T06:47:13Z creation;2023-03-30T06:47:14Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230330064713  20230330070310  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��c�8�1   @�ƿ�.�@0=/��w�c�E���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BrffBw33B��B���B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B˙�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C633C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ DɃ3D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @p�@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Br
=Bv�
B=qB���B���B���B�k�B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C�C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C6)C7��C9��C;�\C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�\Cg��Cj�Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC�HC�HC��{C��{D z=D ��Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DP��DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dq��Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dy �Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=DɀRDɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v+A�|PA�{�A�y>A�p�A�*�A�(XA��dAʫkAʛ�AʊrA�{�A�qvA�jA�c�A�^5A�XEA�PA�M6A�J�A�F�A�D�A�C-A�>�A�8A�2�A�,�A��Aə�A��<A�1�A��ZAŗ�A�бA�F�A� 4A�~A�'A���A�d�A�@OA�~A��tA���A��A�&�A��^A�(XA�$�A��9A�^�A���A�
�A���A�_�A�{JA��A�R�A�h�A�=qA�aA��A�$A�2-A��	A�<jA��A��A���A�fA��tA��;A�)*A��A�m�A�@�A��KA�a�A���A��vA�IA��A�ǮA��A�DgA��A�jA��yA��6A|!Ay��Aw�Ap�UAl  Ai/Af�Ad�Ac�AbP�A`P�A^	lAZ�AXp;AW��AU�APL0AM^�AL+�AK�=AI�jAI~AGB[AGO�AG2�AC�A>1�A?ȴA=�A;_A7�A6xA4�IA3��A2�,A2(�A-L�A(��A$��A"�AAm]Ah
An/A(A=A�AFtA >BA�mA�A �A�Au�A-�A�A$�AeA�jA� A��A�A�PA��A�A7LA-wA3�A�uA	A@A�]A�A��A#:A�AݘA��A��A�A6�A��Al�AJ�A�vAR�Am]A�zA�WAtTAA*�A%A
��A
<�A
4A	�	A	�}A	*0Af�A�A4AO�A$�A�VAĜA��Aj�A�KA�A2�A�A�*Au%A~(AZ�AM�AƨA��APHA��A��AGA�As�A<6A��A/A �MA -A V@���@�x@�e�@��O@�g8@�q@�u�@��@��	@�� @�>�@�H�@��r@�Ɇ@�E�@�Dg@�M�@�S�@��4@��P@�0�@�>B@�M@��@�=q@���@�V@�s�@�p�@�qv@�g�@�^�@���@��@�w�@�@@��@@�6@�E9@�S@��@�1�@�v`@��@�y>@��>@�33@��@���@�ƨ@�!�@�1�@�c�@�"h@�q@��@��@�ߤ@�"�@� �@�]�@�c @�l�@�m]@ߒ:@���@ۨX@�PH@ٟV@�u%@��g@ׂ�@��@��9@ղ-@ա�@ըX@�s�@�q@�	@���@ӛ=@�4�@�q@��@��@��@Ҭ�@Ҷ�@�C-@ѽ�@я�@�b�@�%@��@υ@�33@���@�@��@��@��@ɜ�@�j@�!�@��Z@��d@��g@�S&@��@ƝI@�c @�ƨ@��"@ħ�@Ě�@Ĉ�@�6@��m@Å@�J�@��@��2@�z�@�J@��M@�F�@�;d@�+@�,�@��@���@�tT@�Ft@�%�@���@�S�@���@��I@�j@�GE@�%�@��T@��@@��@�j@��&@��[@�n�@�7�@���@���@��q@���@�rG@�L�@��V@�a�@��o@�ݘ@�@�$t@�N<@��m@��Y@�#:@���@��@��@��@�8@�ی@�J�@��6@��@���@��>@�u@�E�@��x@�l�@��.@�  @�O@��m@��*@��B@�^5@�@�ԕ@��[@��S@�c@�_p@�e,@�6z@�l�@��@�4@��#@���@�Dg@��@��@���@�u%@�_@���@���@�1�@���@�7�@�@��r@��@�]�@�(@��p@�W�@��@���@�o @�=�@��p@�D�@��+@��@��*@�E9@���@�4n@��T@�ƨ@�w2@�E9@�+@��@���@�� @��@��@��$@�qv@��@��@�oi@�V@�C-@�b@�x@�%F@��s@���@�oi@�K^@�7�@�6@�-�@�  @��m@���@���@���@��f@���@��o@�(�@��@���@���@�K�@��h@�~�@�d�@�@��0@�<6@���@��K@�	l@�S@���@��M@��]@�xl@�_@��@���@�J�@��B@��@���@�i�@�D�@���@�A @���@��@��j@�]d@�$@��@���@�S&@�m]@��M@�2a@��	@���@�R�@�	@��@���@�ƨ@���@�k�@��P@��u@�i�@�@�@��+@��@��7@�Dg@��@��[@�Q@�� @���@�+�@���@��h@��@�n�@�M�@�x@��7@��@��@���@���@�GE@�1@��F@��f@��@�ں@��1@�>B@���@�%@��,@��6@��4@���@���@�U�@�/�@�,�@�@���@��@�Ta@��@��@1�@~l�@}�@}�~@}7L@}Q�@}��@|r�@|!@{�@z��@z�8@{�@zi�@y�@yN<@yVm@y\�@xی@xq@w��@wH�@vff@u(�@s�@s$t@r4@q�@q^�@p�@q�h@q��@qx�@q�@qc@p��@o��@p1'@p]d@p  @o�@o��@p7@p/�@p!@p!@p~@p~@o�0@oO@o33@n�@nl�@m�z@m+�@l��@l��@lx@k��@k\)@j��@j�@j�@i�@iT�@i@@i%@h�K@h�z@h�@g��@g��@gj�@eԕ@d��@e;@d�@d7�@c�0@c��@cg�@c,�@c!-@b�8@b�1@a��@a��@a��@`��@`h�@`m�@`l"@_l�@^�@^�1@^}V@^s�@^� @^;�@]ϫ@]�@\��@\M@[��@[&@Z�B@Z��@Z.�@YA @XĜ@XM@W9�@W�@V�,@V�]@V�]@V�b@VR�@U��@U!�@T��@T(�@T%�@TbN@Tr�@TK^@S�@S��@S�@R�b@RH�@Q��@QrG@Q�~@Q�3@Q�@R8�@Q��@Qc�@Q;@P6@O4�@NC�@M@M�"@MJ�@M4@M[W@M��@Mu�@M2a@M`B@L��@Mu�@MB�@Mq@M�@Loi@L"h@K�@K��@K@J�8@J��@Iԕ@Im]@H�@H�u@HD�@H  @G�
@G�w@G�q@G��@Gv`@G@O@G)_@F��@F��@F��@F�r@FQ@E�Z@E�9@EA @E!�@D��@D|�@DS�@DD�@C��@CJ#@B�8@B��@Be@Ao @A�@@�I@@%�@?�W@?g�@>�'@>�x@>}V@>p;@>Ov@>&�@=�@=s�@<��@<��@<��@<��@<M@<~@<�@;�@:�,@:��@:^5@:GE@:e@9��@9�@9�@8��@8l"@8$@7�@7C@6��@6��@6}V@6xl@6_�@6u@5u�@5�@4�p@4bN@3�@3�0@3�P@3iD@3S�@3E9@3�@2�,@2�}@2u%@2;�@2 �@1�^@1[W@1�@0��@0 �@/�	@/K�@/$t@.��@.�m@.�@.c @.H�@.-@-�Z@-�j@-�@-�@-�^@-��@-G�@,��@,y>@+�@+��@+��@+�4@+dZ@+S�@+9�@+8@*�@*�h@*v�@*6�@)��@)��@)Q�@(�@(�e@(S�@(x@'�:@'6z@&�@&�B@&�L@&~�@&H�@&_@%��@%��@%��@%8�@%�@$ѷ@$�e@$w�@$l"@$D�@$@#�@#�0@#a@#4�@"�M@"�2@"�6@"d�@"C�@"�@!��@!ԕ@!@!�@!��@!s�@!N<@!A @!@ �@ �@t�@8@�@�R@xl@&�@�@hs@&�@ \@ \@#�@�@��@��@�@�m@�
@��@x@F�@�8@�@W�@-@�@�T@�n@�S@m]@G�@#�@֡@��@�Y@z�@z�@z�@h�@N�@�]@�{@�@o@�2@��@��@�@�-@��@��@�@�@��@��@r�@Xy@Ft@'R@�@��@�V@�{@>�@�@ȴ@�1@s�@l�@:*@�@@s�@X@!�@�|@�@h�@Z@��@�g@y�@33@)_@$t@"�@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v+A�|PA�{�A�y>A�p�A�*�A�(XA��dAʫkAʛ�AʊrA�{�A�qvA�jA�c�A�^5A�XEA�PA�M6A�J�A�F�A�D�A�C-A�>�A�8A�2�A�,�A��Aə�A��<A�1�A��ZAŗ�A�бA�F�A� 4A�~A�'A���A�d�A�@OA�~A��tA���A��A�&�A��^A�(XA�$�A��9A�^�A���A�
�A���A�_�A�{JA��A�R�A�h�A�=qA�aA��A�$A�2-A��	A�<jA��A��A���A�fA��tA��;A�)*A��A�m�A�@�A��KA�a�A���A��vA�IA��A�ǮA��A�DgA��A�jA��yA��6A|!Ay��Aw�Ap�UAl  Ai/Af�Ad�Ac�AbP�A`P�A^	lAZ�AXp;AW��AU�APL0AM^�AL+�AK�=AI�jAI~AGB[AGO�AG2�AC�A>1�A?ȴA=�A;_A7�A6xA4�IA3��A2�,A2(�A-L�A(��A$��A"�AAm]Ah
An/A(A=A�AFtA >BA�mA�A �A�Au�A-�A�A$�AeA�jA� A��A�A�PA��A�A7LA-wA3�A�uA	A@A�]A�A��A#:A�AݘA��A��A�A6�A��Al�AJ�A�vAR�Am]A�zA�WAtTAA*�A%A
��A
<�A
4A	�	A	�}A	*0Af�A�A4AO�A$�A�VAĜA��Aj�A�KA�A2�A�A�*Au%A~(AZ�AM�AƨA��APHA��A��AGA�As�A<6A��A/A �MA -A V@���@�x@�e�@��O@�g8@�q@�u�@��@��	@�� @�>�@�H�@��r@�Ɇ@�E�@�Dg@�M�@�S�@��4@��P@�0�@�>B@�M@��@�=q@���@�V@�s�@�p�@�qv@�g�@�^�@���@��@�w�@�@@��@@�6@�E9@�S@��@�1�@�v`@��@�y>@��>@�33@��@���@�ƨ@�!�@�1�@�c�@�"h@�q@��@��@�ߤ@�"�@� �@�]�@�c @�l�@�m]@ߒ:@���@ۨX@�PH@ٟV@�u%@��g@ׂ�@��@��9@ղ-@ա�@ըX@�s�@�q@�	@���@ӛ=@�4�@�q@��@��@��@Ҭ�@Ҷ�@�C-@ѽ�@я�@�b�@�%@��@υ@�33@���@�@��@��@��@ɜ�@�j@�!�@��Z@��d@��g@�S&@��@ƝI@�c @�ƨ@��"@ħ�@Ě�@Ĉ�@�6@��m@Å@�J�@��@��2@�z�@�J@��M@�F�@�;d@�+@�,�@��@���@�tT@�Ft@�%�@���@�S�@���@��I@�j@�GE@�%�@��T@��@@��@�j@��&@��[@�n�@�7�@���@���@��q@���@�rG@�L�@��V@�a�@��o@�ݘ@�@�$t@�N<@��m@��Y@�#:@���@��@��@��@�8@�ی@�J�@��6@��@���@��>@�u@�E�@��x@�l�@��.@�  @�O@��m@��*@��B@�^5@�@�ԕ@��[@��S@�c@�_p@�e,@�6z@�l�@��@�4@��#@���@�Dg@��@��@���@�u%@�_@���@���@�1�@���@�7�@�@��r@��@�]�@�(@��p@�W�@��@���@�o @�=�@��p@�D�@��+@��@��*@�E9@���@�4n@��T@�ƨ@�w2@�E9@�+@��@���@�� @��@��@��$@�qv@��@��@�oi@�V@�C-@�b@�x@�%F@��s@���@�oi@�K^@�7�@�6@�-�@�  @��m@���@���@���@��f@���@��o@�(�@��@���@���@�K�@��h@�~�@�d�@�@��0@�<6@���@��K@�	l@�S@���@��M@��]@�xl@�_@��@���@�J�@��B@��@���@�i�@�D�@���@�A @���@��@��j@�]d@�$@��@���@�S&@�m]@��M@�2a@��	@���@�R�@�	@��@���@�ƨ@���@�k�@��P@��u@�i�@�@�@��+@��@��7@�Dg@��@��[@�Q@�� @���@�+�@���@��h@��@�n�@�M�@�x@��7@��@��@���@���@�GE@�1@��F@��f@��@�ں@��1@�>B@���@�%@��,@��6@��4@���@���@�U�@�/�@�,�@�@���@��@�Ta@��@��@1�@~l�@}�@}�~@}7L@}Q�@}��@|r�@|!@{�@z��@z�8@{�@zi�@y�@yN<@yVm@y\�@xی@xq@w��@wH�@vff@u(�@s�@s$t@r4@q�@q^�@p�@q�h@q��@qx�@q�@qc@p��@o��@p1'@p]d@p  @o�@o��@p7@p/�@p!@p!@p~@p~@o�0@oO@o33@n�@nl�@m�z@m+�@l��@l��@lx@k��@k\)@j��@j�@j�@i�@iT�@i@@i%@h�K@h�z@h�@g��@g��@gj�@eԕ@d��@e;@d�@d7�@c�0@c��@cg�@c,�@c!-@b�8@b�1@a��@a��@a��@`��@`h�@`m�@`l"@_l�@^�@^�1@^}V@^s�@^� @^;�@]ϫ@]�@\��@\M@[��@[&@Z�B@Z��@Z.�@YA @XĜ@XM@W9�@W�@V�,@V�]@V�]@V�b@VR�@U��@U!�@T��@T(�@T%�@TbN@Tr�@TK^@S�@S��@S�@R�b@RH�@Q��@QrG@Q�~@Q�3@Q�@R8�@Q��@Qc�@Q;@P6@O4�@NC�@M@M�"@MJ�@M4@M[W@M��@Mu�@M2a@M`B@L��@Mu�@MB�@Mq@M�@Loi@L"h@K�@K��@K@J�8@J��@Iԕ@Im]@H�@H�u@HD�@H  @G�
@G�w@G�q@G��@Gv`@G@O@G)_@F��@F��@F��@F�r@FQ@E�Z@E�9@EA @E!�@D��@D|�@DS�@DD�@C��@CJ#@B�8@B��@Be@Ao @A�@@�I@@%�@?�W@?g�@>�'@>�x@>}V@>p;@>Ov@>&�@=�@=s�@<��@<��@<��@<��@<M@<~@<�@;�@:�,@:��@:^5@:GE@:e@9��@9�@9�@8��@8l"@8$@7�@7C@6��@6��@6}V@6xl@6_�@6u@5u�@5�@4�p@4bN@3�@3�0@3�P@3iD@3S�@3E9@3�@2�,@2�}@2u%@2;�@2 �@1�^@1[W@1�@0��@0 �@/�	@/K�@/$t@.��@.�m@.�@.c @.H�@.-@-�Z@-�j@-�@-�@-�^@-��@-G�@,��@,y>@+�@+��@+��@+�4@+dZ@+S�@+9�@+8@*�@*�h@*v�@*6�@)��@)��@)Q�@(�@(�e@(S�@(x@'�:@'6z@&�@&�B@&�L@&~�@&H�@&_@%��@%��@%��@%8�@%�@$ѷ@$�e@$w�@$l"@$D�@$@#�@#�0@#a@#4�@"�M@"�2@"�6@"d�@"C�@"�@!��@!ԕ@!@!�@!��@!s�@!N<@!A @!@ �@ �@t�@8@�@�R@xl@&�@�@hs@&�@ \@ \@#�@�@��@��@�@�m@�
@��@x@F�@�8@�@W�@-@�@�T@�n@�S@m]@G�@#�@֡@��@�Y@z�@z�@z�@h�@N�@�]@�{@�@o@�2@��@��@�@�-@��@��@�@�@��@��@r�@Xy@Ft@'R@�@��@�V@�{@>�@�@ȴ@�1@s�@l�@:*@�@@s�@X@!�@�|@�@h�@Z@��@�g@y�@33@)_@$t@"�@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	C�B	CaB	C�B	CaB	D�B	E�B	E�B	G�B	HfB	IB	I�B	K)B	L0B	MB	NB	NVB	N�B	N�B	N�B	N�B	OB	N�B	OBB	O\B	OBB	OB	N�B	L�B	DgB	3B	(�B	�B	5?B	��B
?�B
h>B
�HB
�B)BoBNBBEBKB
��B�B3B[B8�BxB��B��B�B��B��B� B�B��BtTBi�Ba�Be,Bi�Bb�BXyB_pBbhBabB]�BW?BH�B;�B.�B�BTB�B
�
B
�BB
�?B
�HB
��B
�,B
�}B
nIB
2�B
%�B
�B	�B	��B	��B	�OB	��B	��B	r�B	h�B	_�B	T�B	OBB	J�B	GEB	E�B	=VB	5tB	5�B	3�B	-�B	(
B	&�B	&B	*0B	$�B	$�B	>�B	GEB	BAB	5�B	w�B	�B	}�B	s�B	kkB	o B	u�B	y�B	v�B	^B	7�B	�B	�B��B��B�B��B	EB	'B	N"B	t9B	}VB	�B	�mB	��B	��B	��B	}�B	�+B	��B	�GB	�4B	��B	�B	oOB	m)B	u�B	��B	��B	�rB	�4B	�B	ݲB	ӏB	��B	��B	�9B	�2B	� B	��B	�_B	�gB	��B	�]B	�`B	�RB	�hB	��B	��B	� B	�\B	��B	��B	�{B	�EB	��B	��B	��B	�9B	�SB	�7B	��B	��B	��B	��B	�yB	�>B	��B	��B	�$B	��B	��B	��B	˒B	��B	�NB	�SB	ڠB	�B	ܒB	�aB	�dB	��B	ңB	�SB	�zB	��B	�
B	�B	ּB	�qB	��B	�vB	�=B	�B	��B	��B	�B	�FB	��B	�?B	��B	��B	��B	��B	��B	�B	�WB	��B	��B	�KB	�WB	�B	�
B	�B	�B	ںB	��B	��B	�zB	�B	�B	�B	�B	�5B	�B	�B	�B	�yB	�yB	�B	��B	�fB	�FB	�B	�TB	��B	��B	�>B	�B	�KB	�*B	�XB	�>B	�
B	�@B	�B	��B	�-B	�B	��B	�LB	�zB	�B	��B	��B	�_B	��B	�B	��B	�B	�=B	��B	��B	�[B	� B	�@B	�B	�@B	՛B	�yB	��B	��B	�B	��B	��B	�IB	ߊB	�vB	�4B	��B	�B	�$B	�B	��B	�qB	�iB	�B	�[B	�B	��B	�oB	�OB	��B	�B	��B	�B	�B	�tB	�B	�B	��B	��B	��B	�*B	�*B	��B	�yB	�RB	��B	�B	��B	�B	�B	�B	�B	�B	�wB	��B	�IB	��B	�B	��B	��B	��B	�`B	��B	�+B	�FB	��B	��B	�LB	�B	��B	��B	�$B	�rB	��B	��B	��B	�0B	�B	��B	��B	��B	�(B	��B	�HB
 �B
 �B
;B
�B
�B
�B	�}B	��B	��B	�B	�B	�wB	�qB	�B	�yB	�B	�*B	�B	�B	�B	�fB	�B	�QB	�B	�[B	��B	��B	�>B	��B	�$B	��B	��B	�xB	��B	��B	�B	�B	��B	��B	�<B	�HB
 4B
oB
 B
�B
aB
�B
B
�B
B
mB
�B
�B
�B
	RB
	�B
	RB

#B

XB

�B

�B
�B
JB
JB
~B
PB
�B
�B
�B
6B
B
�B
JB
�B
�B
�B
jB
PB
pB
�B
�B
�B
�B
�B
�B
B
vB
�B
�B
�B
�B
�B
�B
B
(B
�B
B
B
VB
B
\B
�B
NB
FB
�B
+B
_B
�B
eB
7B
B
B
B
�B
B
�B
1B
B
7B
�B
�B
B
�B
kB
B
qB
B
OB
jB
�B
�B
 B
�B
pB
;B
�B
�B
!�B
"4B
"B
!�B
"NB
!HB
!B
!B
 �B
�B
;B
�B
B
!B
 \B
#�B
#�B
#�B
$�B
%FB
%�B
%�B
%�B
%�B
%�B
%�B
&2B
%�B
%�B
%�B
&2B
'8B
'�B
'�B
(>B
($B
(
B
)yB
)�B
(�B
)B
*�B
+QB
+B
+B
+B
+QB
*eB
+�B
,qB
,�B
,�B
,�B
,=B
+B
+6B
.�B
.}B
.IB
-)B
+6B
*B
*B
)�B
*KB
)*B
'8B
&�B
'�B
(�B
)*B
)�B
)�B
)B
(
B
'8B
&�B
&�B
'�B
(XB
(�B
+B
+kB
-)B
-B
-�B
/ B
2-B
3hB
2�B
2�B
3B
4nB
4�B
6+B
6+B
4�B
2�B
/�B
.cB
.�B
-�B
,�B
-]B
-�B
2�B
5%B
7B
8RB
8�B
7�B
7�B
8�B
:B
:*B
:*B
;�B
<�B
>BB
>]B
>�B
@ B
@�B
A;B
A B
A;B
A�B
BAB
B�B
BuB
B�B
C�B
C�B
D�B
D�B
D�B
C�B
E9B
E�B
EB
D�B
D�B
EB
E�B
EmB
E�B
F�B
G+B
G�B
G�B
G�B
HB
HfB
HfB
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
IB
H�B
H�B
HfB
HB
G�B
HKB
J#B
J�B
KxB
KxB
K�B
LB
K�B
K)B
JrB
J�B
J�B
K^B
J�B
LB
K�B
K�B
K�B
L~B
M�B
M�B
M�B
N"B
N�B
OB
NpB
N�B
O�B
P.B
Q�B
R�B
S�B
UB
UB
T�B
T�B
T{B
TFB
T,B
T{B
U�B
V9B
W�B
XB
WsB
V�B
VSB
U2B
TFB
S�B
S�B
S�B
TFB
T�B
VB
VSB
VmB
X�B
X_B
ZQB
ZkB
ZkB
ZkB
Z�B
[qB
[qB
[�B
[�B
[�B
\�B
]B
]IB
^B
^B
^5B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^OB
^jB
^jB
^�B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_;B
_B
_�B
_�B
`\B
`�B
`�B
a-B
aHB
abB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
b�B
bB
b�B
cnB
cTB
cnB
b�B
b4B
bB
a�B
a�B
a|B
bB
b�B
c:B
cB
b�B
b�B
b�B
cTB
cnB
c�B
cTB
c�B
cnB
c�B
c�B
c�B
d�B
dtB
dZB
dZB
d�B
eB
eB
e`B
f�B
fLB
gB
gB
gmB
gB
g�B
g�B
h
B
h
B
h�B
h�B
i*B
iyB
iDB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
iDB
i�B
j0B
i�B
i�B
j�B
kQB
kkB
k�B
k�B
k�B
lB
l"B
lB
l=B
l�B
l�B
l�B
mB
m]B
m�B
nB
nIB
n�B
n�B
o5B
o�B
o�B
o�B
o�B
p;B
pUB
p�B
p�B
p�B
p�B
qAB
q[B
qvB
q�B
q�B
q�B
rB
rB
rGB
raB
r�B
r�B
sMB
s3B
shB
s�B
s�B
s�B
tB
tB
tB
tB
tB
tTB
tTB
tTB
t�B
t�B
utB
u�B
u�B
vFB
vFB
vzB
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xRB
x8B
xlB
x8B
xRB
x�B
x�B
y	B
yXB
yrB
y�B
y�B
zB
zB
zxB
z�B
z�B
{B
{dB
{dB
{B
{B
{dB
{B
{B
{�B
|jB
|�B
|�B
|�B
|�B
}"B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
.B
.B
HB
HB
}B
�B
�B
� B
�B
�iB
��B
��B
��B
�B
�B
�UB
��B
��B
�B
�AB
�[B
�uB
��B
��B
�B
��B
��B
�gB
��B
��B
�gB
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	C�B	CaB	C�B	CaB	D�B	E�B	E�B	G�B	HfB	IB	I�B	K)B	L0B	MB	NB	NVB	N�B	N�B	N�B	N�B	OB	N�B	OBB	O\B	OBB	OB	N�B	L�B	DgB	3B	(�B	�B	5?B	��B
?�B
h>B
�HB
�B)BoBNBBEBKB
��B�B3B[B8�BxB��B��B�B��B��B� B�B��BtTBi�Ba�Be,Bi�Bb�BXyB_pBbhBabB]�BW?BH�B;�B.�B�BTB�B
�
B
�BB
�?B
�HB
��B
�,B
�}B
nIB
2�B
%�B
�B	�B	��B	��B	�OB	��B	��B	r�B	h�B	_�B	T�B	OBB	J�B	GEB	E�B	=VB	5tB	5�B	3�B	-�B	(
B	&�B	&B	*0B	$�B	$�B	>�B	GEB	BAB	5�B	w�B	�B	}�B	s�B	kkB	o B	u�B	y�B	v�B	^B	7�B	�B	�B��B��B�B��B	EB	'B	N"B	t9B	}VB	�B	�mB	��B	��B	��B	}�B	�+B	��B	�GB	�4B	��B	�B	oOB	m)B	u�B	��B	��B	�rB	�4B	�B	ݲB	ӏB	��B	��B	�9B	�2B	� B	��B	�_B	�gB	��B	�]B	�`B	�RB	�hB	��B	��B	� B	�\B	��B	��B	�{B	�EB	��B	��B	��B	�9B	�SB	�7B	��B	��B	��B	��B	�yB	�>B	��B	��B	�$B	��B	��B	��B	˒B	��B	�NB	�SB	ڠB	�B	ܒB	�aB	�dB	��B	ңB	�SB	�zB	��B	�
B	�B	ּB	�qB	��B	�vB	�=B	�B	��B	��B	�B	�FB	��B	�?B	��B	��B	��B	��B	��B	�B	�WB	��B	��B	�KB	�WB	�B	�
B	�B	�B	ںB	��B	��B	�zB	�B	�B	�B	�B	�5B	�B	�B	�B	�yB	�yB	�B	��B	�fB	�FB	�B	�TB	��B	��B	�>B	�B	�KB	�*B	�XB	�>B	�
B	�@B	�B	��B	�-B	�B	��B	�LB	�zB	�B	��B	��B	�_B	��B	�B	��B	�B	�=B	��B	��B	�[B	� B	�@B	�B	�@B	՛B	�yB	��B	��B	�B	��B	��B	�IB	ߊB	�vB	�4B	��B	�B	�$B	�B	��B	�qB	�iB	�B	�[B	�B	��B	�oB	�OB	��B	�B	��B	�B	�B	�tB	�B	�B	��B	��B	��B	�*B	�*B	��B	�yB	�RB	��B	�B	��B	�B	�B	�B	�B	�B	�wB	��B	�IB	��B	�B	��B	��B	��B	�`B	��B	�+B	�FB	��B	��B	�LB	�B	��B	��B	�$B	�rB	��B	��B	��B	�0B	�B	��B	��B	��B	�(B	��B	�HB
 �B
 �B
;B
�B
�B
�B	�}B	��B	��B	�B	�B	�wB	�qB	�B	�yB	�B	�*B	�B	�B	�B	�fB	�B	�QB	�B	�[B	��B	��B	�>B	��B	�$B	��B	��B	�xB	��B	��B	�B	�B	��B	��B	�<B	�HB
 4B
oB
 B
�B
aB
�B
B
�B
B
mB
�B
�B
�B
	RB
	�B
	RB

#B

XB

�B

�B
�B
JB
JB
~B
PB
�B
�B
�B
6B
B
�B
JB
�B
�B
�B
jB
PB
pB
�B
�B
�B
�B
�B
�B
B
vB
�B
�B
�B
�B
�B
�B
B
(B
�B
B
B
VB
B
\B
�B
NB
FB
�B
+B
_B
�B
eB
7B
B
B
B
�B
B
�B
1B
B
7B
�B
�B
B
�B
kB
B
qB
B
OB
jB
�B
�B
 B
�B
pB
;B
�B
�B
!�B
"4B
"B
!�B
"NB
!HB
!B
!B
 �B
�B
;B
�B
B
!B
 \B
#�B
#�B
#�B
$�B
%FB
%�B
%�B
%�B
%�B
%�B
%�B
&2B
%�B
%�B
%�B
&2B
'8B
'�B
'�B
(>B
($B
(
B
)yB
)�B
(�B
)B
*�B
+QB
+B
+B
+B
+QB
*eB
+�B
,qB
,�B
,�B
,�B
,=B
+B
+6B
.�B
.}B
.IB
-)B
+6B
*B
*B
)�B
*KB
)*B
'8B
&�B
'�B
(�B
)*B
)�B
)�B
)B
(
B
'8B
&�B
&�B
'�B
(XB
(�B
+B
+kB
-)B
-B
-�B
/ B
2-B
3hB
2�B
2�B
3B
4nB
4�B
6+B
6+B
4�B
2�B
/�B
.cB
.�B
-�B
,�B
-]B
-�B
2�B
5%B
7B
8RB
8�B
7�B
7�B
8�B
:B
:*B
:*B
;�B
<�B
>BB
>]B
>�B
@ B
@�B
A;B
A B
A;B
A�B
BAB
B�B
BuB
B�B
C�B
C�B
D�B
D�B
D�B
C�B
E9B
E�B
EB
D�B
D�B
EB
E�B
EmB
E�B
F�B
G+B
G�B
G�B
G�B
HB
HfB
HfB
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
IB
H�B
H�B
HfB
HB
G�B
HKB
J#B
J�B
KxB
KxB
K�B
LB
K�B
K)B
JrB
J�B
J�B
K^B
J�B
LB
K�B
K�B
K�B
L~B
M�B
M�B
M�B
N"B
N�B
OB
NpB
N�B
O�B
P.B
Q�B
R�B
S�B
UB
UB
T�B
T�B
T{B
TFB
T,B
T{B
U�B
V9B
W�B
XB
WsB
V�B
VSB
U2B
TFB
S�B
S�B
S�B
TFB
T�B
VB
VSB
VmB
X�B
X_B
ZQB
ZkB
ZkB
ZkB
Z�B
[qB
[qB
[�B
[�B
[�B
\�B
]B
]IB
^B
^B
^5B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^OB
^jB
^jB
^�B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_;B
_B
_�B
_�B
`\B
`�B
`�B
a-B
aHB
abB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
b�B
bB
b�B
cnB
cTB
cnB
b�B
b4B
bB
a�B
a�B
a|B
bB
b�B
c:B
cB
b�B
b�B
b�B
cTB
cnB
c�B
cTB
c�B
cnB
c�B
c�B
c�B
d�B
dtB
dZB
dZB
d�B
eB
eB
e`B
f�B
fLB
gB
gB
gmB
gB
g�B
g�B
h
B
h
B
h�B
h�B
i*B
iyB
iDB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
iDB
i�B
j0B
i�B
i�B
j�B
kQB
kkB
k�B
k�B
k�B
lB
l"B
lB
l=B
l�B
l�B
l�B
mB
m]B
m�B
nB
nIB
n�B
n�B
o5B
o�B
o�B
o�B
o�B
p;B
pUB
p�B
p�B
p�B
p�B
qAB
q[B
qvB
q�B
q�B
q�B
rB
rB
rGB
raB
r�B
r�B
sMB
s3B
shB
s�B
s�B
s�B
tB
tB
tB
tB
tB
tTB
tTB
tTB
t�B
t�B
utB
u�B
u�B
vFB
vFB
vzB
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xRB
x8B
xlB
x8B
xRB
x�B
x�B
y	B
yXB
yrB
y�B
y�B
zB
zB
zxB
z�B
z�B
{B
{dB
{dB
{B
{B
{dB
{B
{B
{�B
|jB
|�B
|�B
|�B
|�B
}"B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
.B
.B
HB
HB
}B
�B
�B
� B
�B
�iB
��B
��B
��B
�B
�B
�UB
��B
��B
�B
�AB
�[B
�uB
��B
��B
�B
��B
��B
�gB
��B
��B
�gB
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230330064712  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230330064713  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230330064714  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230330064714                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230330064715  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230330064715  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230330070310                      G�O�G�O�G�O�                