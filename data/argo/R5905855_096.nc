CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:27:40Z creation;2022-06-04T19:27:40Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192740  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               `A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٖ����k1   @ٖ��F*@,����l��d*��+1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   AffA@  A`  A�  A�  A���A�33A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  C �C�3C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&L�C'��C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CVL�CW�fCZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�C3Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@s�@��@��A��A>�\A^�\A~�\A�G�A��HA�z�A�z�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B�Bמ�B۞�B���B���B���B���B���B���B���B���C �C�)C�\C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C&5�C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CT�CV5�CW�\CY��C[��C]��C_��Ca�\Cc�\Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD� RD�@RD�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��KAܼjA�^�A��A��ZA���Aۼ�Aۻ�Aۼ�A۾Aۥ�Aۛ=A�w2A�:�A�VA���Aڽ<Aڑ�A�}�A�n�A�`�A�XEA�F�A�5tA�!�A�oA��A�� A�ݘA��#AٽA٨$Aه�A�'RA�B�A�A��A��A��0A˂�AȨ$A�8RA� A�r�A�\)A�A�'�A�[�A�MA�B�A�c�A�"�A���A��hA���A��A�7LA��A��pA�XA��A�9�A�<�A�FA�D�A���A��uA�2�A���A�.�A�^5A���A��>A���A���A�~A�уA��(A���A���A�OA��]A���A��A���A��-A���A��A���A�%�A�SAzv`As�Al�}Ag��AdX�Aa��A^�bA\K^AY�AUqATOAS;dAR=�AP��AOALԕAL�AJg�AH�)AF5�AB�A@��A=��A;��A9FA8!�A6��A4�A4ffA3�dA2�A1�KA1A0�dA0,=A.��A-�A-e�A-�A,�A,$�A+��A+/�A*�SA*��A*�SA*M�A)��A(��A(>BA'�dA%�nA$�A$�A#��A#��A#MjA$TaA$�hA$)_A"�A"*0A"��A"��A"O�A"oA!�`A!��A �A GAN�A��AE�A�oA�\A��A��A��A�5AVmA1A�zA��Ac A7�A�eA�hA4A)_A�BA͟A��A?}A	A��A�.A{JAr�AE9A2�A�4A/AݘA��A�AV�AS&APHAA	A��AیA��AxlACA�>A�hAv`A'RA1AU2A��A*�A�AiDA�KA��Ad�AK�A�A
�A
�1A
(�A	f�A��A��A(�A�9AHAOA��A.�A��AMAu�A	A0UA�A�A�"Au�A'�A �hA ��A e,A S�A 2�A %�A eA V@��A@�w2@���@�>B@��M@�M@�0�@���@�W�@��@�0U@���@��"@�	l@��@�ѷ@���@�$@�L�@��x@�t�@�$�@�zx@�dZ@�;d@���@�k�@��|@��@�+@�K�@�!@��K@�&�@��@��E@�w2@�q@�ی@�L@�I�@��@�˒@�h@�Mj@�@䎊@��@�͟@��d@�(@�oi@�0U@ߙ�@�9�@��@��@ި�@�bN@�ݘ@�V@�6�@ۼ@�C�@��H@��@�{J@�Y@���@�u�@�W�@�+k@�4@ք�@��@՘�@�/�@���@� i@�@ԗ�@�V@Ӫ�@��X@���@�خ@�خ@��d@��@�+k@��@ϯ�@�Z�@��@ι�@Ό�@�+k@��Q@�j@��@̶�@�@�o�@�@@ʾ@�M�@ɱ[@�"�@���@�Xy@���@ƴ9@�2�@�\�@İ�@à'@�?}@���@�Ft@���@���@��'@���@��@�7�@�P�@���@�:*@��)@���@�^�@�4@��@��2@��H@��@���@�@���@�}�@��s@�Q�@��&@�zx@�+�@���@�i�@��]@���@�dZ@�V@��@���@�)_@���@�H�@�b@���@���@�5�@��p@���@���@�~@���@�&�@���@�I�@�M@��}@��^@�m]@�n�@��@�o @�A @���@��@�i�@�:�@�@��a@��@���@�L0@��N@�F@��@��@��$@���@��W@���@��4@�Z@��Q@���@�b�@�C�@��@�}V@��@���@��4@�=�@��@��[@���@�w�@�4n@��d@�J#@��@���@�ی@���@���@��F@�:�@��^@�b�@�C@�}V@�u@���@��@�|�@��@���@�o�@�Z�@�%@���@�$@���@��F@�Z�@�4�@��@�l"@���@�a�@� i@��`@��O@�V@���@�4@���@��.@�oi@�J�@�4@���@���@�K�@�@@���@��p@���@��A@�j@�H�@��@���@���@��z@��S@�?}@��@���@��I@�v�@�I�@���@���@�W?@�C@��@��\@�8�@��a@�]�@���@��u@�	@��r@�� @���@�8�@��@���@�ں@���@�e�@�4n@�%�@��@��^@���@�v`@�%F@��9@���@���@�m�@��@���@�s�@�j@�Y�@�8�@��@�ߤ@�͟@��$@��6@��@��A@��A@�IR@��@���@�u�@�=q@��@���@��d@��	@�s@�]�@���@�v�@��@��N@��@�Z�@�33@��[@���@���@��4@�g8@�?@�!@�@a@C�@@~��@~u%@~�@}�@}��@}/@|��@|<�@|(�@{�a@{��@z��@z@y��@y�S@yu�@y4@y+@x��@xɆ@x$@w�&@w�@vTa@u��@u��@ue,@uY�@t��@t*�@s��@s8@r��@r0U@q�d@qw2@qV@p�j@pj@o~�@o$t@n�}@nOv@m�@mrG@mF@l�`@lU2@kخ@k�	@j�8@j�@iVm@h~(@h,=@h�@g�V@gX�@g1�@f��@f��@e��@d�f@dɆ@d�O@dw�@c��@c�{@cRT@c�@b�@b�@b��@b6�@a�o@a�N@a��@a�@`m�@_�;@_�
@_�}@_�$@_F�@_�@^ߤ@^�m@^��@^YK@]��@]��@]c@]A @\��@\��@\w�@\%�@[�@[{J@[P�@Z��@Zں@Zv�@Z!�@Y�@Y�=@Y�M@Y\�@Y8�@Y�@X��@X��@X�U@X�9@X��@X~(@Xw�@Xh�@X7�@Wخ@We�@V�H@Vp;@U��@UN<@T��@TXy@T!@S�@S��@S�@S��@SZ�@R�@R��@R.�@R@Q��@Q@Q��@Qhs@Q \@P�K@P��@P��@P1'@O]�@O�@N�<@NGE@N($@M�@M��@MX@M?}@M(�@L��@L��@Le�@L2�@K�;@K�:@Kn/@KE9@K�@J�c@J��@I��@I*0@H��@H�U@H��@G��@G;d@F��@F��@FJ@E��@E5�@Dی@D�I@D�.@D�@DS�@C�Q@C\)@C
=@B��@B��@BYK@A��@A��@A��@A}�@AY�@A<6@A0�@A�@@�5@@�I@@:�@?�$@>�@>��@>��@>q�@>$�@>�@=�^@=X@=!�@=@@<�	@<��@<N�@<x@;�a@;�P@;S@:��@:L0@9��@9��@9o @8��@8j@87@7�@7�K@7&@6��@6ں@6�L@6_�@5�@5��@5^�@5@@4�)@4/�@4x@3�r@3��@3�@3��@3n/@3W?@3�@2��@2YK@1�@1�@1��@1��@1T�@0�@0j@0b@/��@/y�@.�]@.u%@-��@-N<@-2a@-;@,��@,�@,��@,1'@+�F@+|�@+8@*�"@*�s@*��@*xl@*\�@*�@)��@)�^@)�@)��@)zx@)J�@)!�@)�@(��@(�z@(z�@(Q�@(6@(�@(�@'��@'l�@'=@&�b@&}V@%�d@%��@%B�@%q@%�@%;@$7�@#iD@#4�@"�B@"��@"_�@"�@!�@!�t@!�@!X@!Dg@!+�@!V@ ��@ �@ �@ :�@�@�{@@O@�@(@ߤ@��@��@n�@	@��@�~@`B@+�@��@��@]d@G@��@�}@�	@�@�@z@@�@�.@@\�@5�@(�@�5@�@"h@�6@v`@]�@�@�@ߤ@�@v�@�N@�~@j@B�@+@�@�f@��@�@�.@"h@�@��@�@1�@�@�c@��@�X@�b@��@C�@5?@�@�Z@�#@��@5�@��@��@M@ �@��@��@��@ƨ@�0@�F@��@b�@F�@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��KAܼjA�^�A��A��ZA���Aۼ�Aۻ�Aۼ�A۾Aۥ�Aۛ=A�w2A�:�A�VA���Aڽ<Aڑ�A�}�A�n�A�`�A�XEA�F�A�5tA�!�A�oA��A�� A�ݘA��#AٽA٨$Aه�A�'RA�B�A�A��A��A��0A˂�AȨ$A�8RA� A�r�A�\)A�A�'�A�[�A�MA�B�A�c�A�"�A���A��hA���A��A�7LA��A��pA�XA��A�9�A�<�A�FA�D�A���A��uA�2�A���A�.�A�^5A���A��>A���A���A�~A�уA��(A���A���A�OA��]A���A��A���A��-A���A��A���A�%�A�SAzv`As�Al�}Ag��AdX�Aa��A^�bA\K^AY�AUqATOAS;dAR=�AP��AOALԕAL�AJg�AH�)AF5�AB�A@��A=��A;��A9FA8!�A6��A4�A4ffA3�dA2�A1�KA1A0�dA0,=A.��A-�A-e�A-�A,�A,$�A+��A+/�A*�SA*��A*�SA*M�A)��A(��A(>BA'�dA%�nA$�A$�A#��A#��A#MjA$TaA$�hA$)_A"�A"*0A"��A"��A"O�A"oA!�`A!��A �A GAN�A��AE�A�oA�\A��A��A��A�5AVmA1A�zA��Ac A7�A�eA�hA4A)_A�BA͟A��A?}A	A��A�.A{JAr�AE9A2�A�4A/AݘA��A�AV�AS&APHAA	A��AیA��AxlACA�>A�hAv`A'RA1AU2A��A*�A�AiDA�KA��Ad�AK�A�A
�A
�1A
(�A	f�A��A��A(�A�9AHAOA��A.�A��AMAu�A	A0UA�A�A�"Au�A'�A �hA ��A e,A S�A 2�A %�A eA V@��A@�w2@���@�>B@��M@�M@�0�@���@�W�@��@�0U@���@��"@�	l@��@�ѷ@���@�$@�L�@��x@�t�@�$�@�zx@�dZ@�;d@���@�k�@��|@��@�+@�K�@�!@��K@�&�@��@��E@�w2@�q@�ی@�L@�I�@��@�˒@�h@�Mj@�@䎊@��@�͟@��d@�(@�oi@�0U@ߙ�@�9�@��@��@ި�@�bN@�ݘ@�V@�6�@ۼ@�C�@��H@��@�{J@�Y@���@�u�@�W�@�+k@�4@ք�@��@՘�@�/�@���@� i@�@ԗ�@�V@Ӫ�@��X@���@�خ@�خ@��d@��@�+k@��@ϯ�@�Z�@��@ι�@Ό�@�+k@��Q@�j@��@̶�@�@�o�@�@@ʾ@�M�@ɱ[@�"�@���@�Xy@���@ƴ9@�2�@�\�@İ�@à'@�?}@���@�Ft@���@���@��'@���@��@�7�@�P�@���@�:*@��)@���@�^�@�4@��@��2@��H@��@���@�@���@�}�@��s@�Q�@��&@�zx@�+�@���@�i�@��]@���@�dZ@�V@��@���@�)_@���@�H�@�b@���@���@�5�@��p@���@���@�~@���@�&�@���@�I�@�M@��}@��^@�m]@�n�@��@�o @�A @���@��@�i�@�:�@�@��a@��@���@�L0@��N@�F@��@��@��$@���@��W@���@��4@�Z@��Q@���@�b�@�C�@��@�}V@��@���@��4@�=�@��@��[@���@�w�@�4n@��d@�J#@��@���@�ی@���@���@��F@�:�@��^@�b�@�C@�}V@�u@���@��@�|�@��@���@�o�@�Z�@�%@���@�$@���@��F@�Z�@�4�@��@�l"@���@�a�@� i@��`@��O@�V@���@�4@���@��.@�oi@�J�@�4@���@���@�K�@�@@���@��p@���@��A@�j@�H�@��@���@���@��z@��S@�?}@��@���@��I@�v�@�I�@���@���@�W?@�C@��@��\@�8�@��a@�]�@���@��u@�	@��r@�� @���@�8�@��@���@�ں@���@�e�@�4n@�%�@��@��^@���@�v`@�%F@��9@���@���@�m�@��@���@�s�@�j@�Y�@�8�@��@�ߤ@�͟@��$@��6@��@��A@��A@�IR@��@���@�u�@�=q@��@���@��d@��	@�s@�]�@���@�v�@��@��N@��@�Z�@�33@��[@���@���@��4@�g8@�?@�!@�@a@C�@@~��@~u%@~�@}�@}��@}/@|��@|<�@|(�@{�a@{��@z��@z@y��@y�S@yu�@y4@y+@x��@xɆ@x$@w�&@w�@vTa@u��@u��@ue,@uY�@t��@t*�@s��@s8@r��@r0U@q�d@qw2@qV@p�j@pj@o~�@o$t@n�}@nOv@m�@mrG@mF@l�`@lU2@kخ@k�	@j�8@j�@iVm@h~(@h,=@h�@g�V@gX�@g1�@f��@f��@e��@d�f@dɆ@d�O@dw�@c��@c�{@cRT@c�@b�@b�@b��@b6�@a�o@a�N@a��@a�@`m�@_�;@_�
@_�}@_�$@_F�@_�@^ߤ@^�m@^��@^YK@]��@]��@]c@]A @\��@\��@\w�@\%�@[�@[{J@[P�@Z��@Zں@Zv�@Z!�@Y�@Y�=@Y�M@Y\�@Y8�@Y�@X��@X��@X�U@X�9@X��@X~(@Xw�@Xh�@X7�@Wخ@We�@V�H@Vp;@U��@UN<@T��@TXy@T!@S�@S��@S�@S��@SZ�@R�@R��@R.�@R@Q��@Q@Q��@Qhs@Q \@P�K@P��@P��@P1'@O]�@O�@N�<@NGE@N($@M�@M��@MX@M?}@M(�@L��@L��@Le�@L2�@K�;@K�:@Kn/@KE9@K�@J�c@J��@I��@I*0@H��@H�U@H��@G��@G;d@F��@F��@FJ@E��@E5�@Dی@D�I@D�.@D�@DS�@C�Q@C\)@C
=@B��@B��@BYK@A��@A��@A��@A}�@AY�@A<6@A0�@A�@@�5@@�I@@:�@?�$@>�@>��@>��@>q�@>$�@>�@=�^@=X@=!�@=@@<�	@<��@<N�@<x@;�a@;�P@;S@:��@:L0@9��@9��@9o @8��@8j@87@7�@7�K@7&@6��@6ں@6�L@6_�@5�@5��@5^�@5@@4�)@4/�@4x@3�r@3��@3�@3��@3n/@3W?@3�@2��@2YK@1�@1�@1��@1��@1T�@0�@0j@0b@/��@/y�@.�]@.u%@-��@-N<@-2a@-;@,��@,�@,��@,1'@+�F@+|�@+8@*�"@*�s@*��@*xl@*\�@*�@)��@)�^@)�@)��@)zx@)J�@)!�@)�@(��@(�z@(z�@(Q�@(6@(�@(�@'��@'l�@'=@&�b@&}V@%�d@%��@%B�@%q@%�@%;@$7�@#iD@#4�@"�B@"��@"_�@"�@!�@!�t@!�@!X@!Dg@!+�@!V@ ��@ �@ �@ :�@�@�{@@O@�@(@ߤ@��@��@n�@	@��@�~@`B@+�@��@��@]d@G@��@�}@�	@�@�@z@@�@�.@@\�@5�@(�@�5@�@"h@�6@v`@]�@�@�@ߤ@�@v�@�N@�~@j@B�@+@�@�f@��@�@�.@"h@�@��@�@1�@�@�c@��@�X@�b@��@C�@5?@�@�Z@�#@��@5�@��@��@M@ �@��@��@��@ƨ@�0@�F@��@b�@F�@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	0B	/�B	.}B	,"B	+B	+6B	-B	0;B	3�B	GB	b�B	�mB
�B
NVB
�#B
��B
ĜB
żB
�B
ĶB
��B
�GB
�[B
�oB
��B
�B
�cB
��B
��B
�"B
��B
�B
��B
��B
��B
|�B
��B
�;B
ȴB
�B�B(XBHfBQ4Bc�By>B��B�FB�iB�uBԕB�$B�WB�B B�B��B��B�\B��B�}B�}B�QB� BՁB�B�B��B��B��B�aB�)B�:B�B~�Bl"B^BN"B6`B�B
��B
�|B
�?B
�PB
iyB
V�B
?�B
0�B
)�B
#B
9B	�iB	�UB	�KB	}<B	n}B	a�B	V�B	J�B	AoB	5�B	1AB	/5B	,WB	)B	"�B	/B	kB	�B	B	�B	CB	$�B	�B	&�B	,�B	)yB	%FB	,�B	./B	0!B	1�B	5%B	9XB	=B	@�B	H�B	QB	Z7B	c�B	h�B	r�B	z*B	�'B	��B	��B	�9B	�B	��B	�UB	��B	�B	��B	�B	�&B	��B	��B	��B	��B
�B
gB
�B
1B
FB
 �B
�B
dB
�B
B
�B
�B
�B
�B
)B
EB
;B
'8B
-�B
3�B
2�B
/�B
.IB
-)B
,WB
+QB
*0B
'RB
!�B
#B
B
{B
�B
2B
�B
QB
7B
5B
"hB
'B
.�B
49B
>�B
>�B
AoB
H�B
I�B
J�B
K�B
MB
O\B
P.B
PB
P.B
P�B
P}B
P�B
P.B
P�B
O�B
OB
K�B
H1B
F?B
CB
>]B
<B
9�B
4nB
4B
5%B
6FB
6+B
5�B
33B
0!B
-wB
+QB
(�B
&�B
OB
YB
�B
�B
�B
�B
 B
TB
mB
�B
)B
CB
�B
�B
/B
�B
�B
�B
�B
�B
�B
�B
�B
�B
qB
�B
eB
�B
�B
�B
�B
YB
#B
=B
WB
VB
 �B
!HB
#�B
$tB
"�B
!bB
 vB
�B
�B
5B
IB
B
�B
B
sB
�B
�B
�B
FB
&B
�B
�B
HB
�B
B
�B
B
�B
�B
vB
BB
�B
�B
�B
�B
B
�B
�B
6B
�B
�B
�B
�B
�B
VB
"B
�B
0B
xB
B
�B
B

�B

�B
	�B
�B
fB
�B
�B
�B
�B
�B
9B
�B
_B
�B
�B
�B
?B
�B
�B
aB
GB
aB
�B
B
B
�B
B
�B
�B
aB
�B
GB
B
�B
B
AB
'B
B
-B
�B
gB
B
B
gB
�B
B
 �B
 �B	��B
 iB
  B	��B	��B	��B	��B	��B
 B
�B
�B
 �B
 OB
 B
 4B
 4B
 �B
 �B
 B
B
B
MB
SB
�B
YB
B
YB
%B
�B
mB
mB
?B
B
�B
zB
zB
zB
B
EB
�B
B
�B
�B
�B
�B
�B
�B
�B
B
?B
�B
B
�B
_B
zB
_B
+B
B
B
fB
�B
�B
	lB
	�B
	RB
	RB
	lB

XB

�B

=B

rB
DB
�B
�B
�B
�B
xB
~B
�B
B
PB
�B
VB
pB
�B
B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
,B
{B
�B
�B
gB
�B
�B
�B
�B
SB
?B
�B
�B
B
�B
�B
B
yB
EB
1B
eB
1B
1B
�B
�B
WB
�B
�B
�B
�B
�B
=B
#B
WB
B
CB
�B
�B
~B
�B
B
OB
�B
�B
!B
;B
�B
pB
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!|B
!�B
!�B
"B
"B
"4B
"�B
#B
# B
#nB
#�B
#�B
$@B
$�B
$�B
%`B
%�B
&fB
&�B
&�B
&�B
&�B
'B
'B
'B
'mB
'8B
&�B
&�B
&�B
'B
'mB
'8B
'8B
'�B
'�B
'�B
'�B
(sB
)B
*KB
*B
+6B
+6B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+QB
+B
+B
+B
+QB
+�B
,=B
,�B
,�B
,�B
-wB
-�B
/5B
/iB
/�B
/�B
/�B
0UB
0;B
0�B
0�B
0�B
0�B
1B
1B
1vB
1�B
2B
2B
2aB
2�B
2�B
2�B
2�B
3B
3�B
4B
4B
4B
4TB
4nB
4�B
5�B
6+B
6�B
72B
72B
7B
6�B
6�B
88B
7�B
7�B
9	B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
:*B
:xB
:�B
:�B
:�B
:�B
:�B
;�B
;B
;JB
;�B
<B
<PB
<PB
<�B
<�B
=<B
=VB
=�B
>]B
>�B
?B
?B
?B
?cB
?�B
?}B
?�B
?�B
@�B
A B
A;B
AUB
AUB
A�B
BAB
B�B
B�B
B�B
B�B
B�B
C-B
CaB
CGB
CaB
C�B
DB
D�B
D�B
DgB
D�B
EB
ESB
EmB
E�B
E�B
E�B
F�B
F�B
F�B
GB
GB
GB
G+B
GzB
G�B
G�B
HB
HfB
HfB
H�B
IB
IlB
IlB
I�B
I�B
I�B
I�B
J	B
J#B
J=B
J=B
J=B
JrB
JXB
J=B
JXB
J�B
J�B
KB
K^B
K�B
L0B
L�B
MB
M6B
MPB
MjB
M�B
M�B
M�B
N"B
NVB
N�B
N�B
N�B
N�B
N�B
O(B
OBB
OvB
O\B
OBB
O�B
P.B
O�B
P.B
PHB
PHB
PHB
P}B
P�B
Q B
QB
Q B
Q4B
Q�B
Q�B
RB
R B
RTB
R�B
R�B
R�B
R�B
SB
S@B
S&B
R�B
R�B
S�B
S�B
S�B
T{B
T�B
U�B
UgB
U�B
U�B
U�B
U�B
U�B
VB
V�B
WYB
WYB
WsB
W�B
XEB
X_B
X_B
X�B
X�B
YB
YB
Y1B
Y1B
YeB
YB
Y�B
ZQB
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[qB
[qB
[WB
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]dB
]~B
]~B
^B
^jB
^�B
^�B
^jB
_!B
_!B
_!B
_VB
_VB
_�B
_�B
`B
`'B
`vB
`�B
`�B
`�B
aB
a-B
aHB
aHB
aHB
a|B
abB
abB
a�B
bB
a�B
b4B
bNB
b�B
c B
c�B
c�B
c�B
dZB
dZB
d�B
d�B
d�B
d�B
e,B
e,B
eB
e`B
ezB
e�B
e�B
fB
fB
fLB
fLB
ffB
f�B
gB
gB
gB
gB
gB
g8B
g8B
g8B
g�B
g�B
h
B
h$B
h>B
h>B
h>B
h�B
h�B
h�B
iDB
i*B
i�B
i�B
jKB
jeB
jKB
j0B
j�B
k�B
k�B
lB
l"B
l=B
l�B
l�B
l�B
l�B
mB
m)B
m)B
mCB
m]B
mwB
m�B
m�B
nB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
oB
oOB
o�B
o�B
o�B
pB
pUB
p;B
p�B
p�B
p�B
qB
qAB
q�B
q�B
r-B
rGB
r|B
r�B
r�B
sB
r�B
s3B
s�B
s�B
t9B
tnB
tnB
t�B
t�B
t�B
t�B
u%B
u�B
vB
v+B
v`B
vzB
v`B
v�B
v�B
v�B
v�B
wfB
wfB
w�B
w�B
xRB
xRB
xlB
xRB
xlB
x�B
x�B
y	B
y	B
y	B
y>B
y>B
yXB
y�B
y�B
zDB
z�B
z�B
{B
{0B
{JB
{JB
{dB
{JB
{�B
{�B
{�B
|6B
|P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	0B	/�B	.}B	,"B	+B	+6B	-B	0;B	3�B	GB	b�B	�mB
�B
NVB
�#B
��B
ĜB
żB
�B
ĶB
��B
�GB
�[B
�oB
��B
�B
�cB
��B
��B
�"B
��B
�B
��B
��B
��B
|�B
��B
�;B
ȴB
�B�B(XBHfBQ4Bc�By>B��B�FB�iB�uBԕB�$B�WB�B B�B��B��B�\B��B�}B�}B�QB� BՁB�B�B��B��B��B�aB�)B�:B�B~�Bl"B^BN"B6`B�B
��B
�|B
�?B
�PB
iyB
V�B
?�B
0�B
)�B
#B
9B	�iB	�UB	�KB	}<B	n}B	a�B	V�B	J�B	AoB	5�B	1AB	/5B	,WB	)B	"�B	/B	kB	�B	B	�B	CB	$�B	�B	&�B	,�B	)yB	%FB	,�B	./B	0!B	1�B	5%B	9XB	=B	@�B	H�B	QB	Z7B	c�B	h�B	r�B	z*B	�'B	��B	��B	�9B	�B	��B	�UB	��B	�B	��B	�B	�&B	��B	��B	��B	��B
�B
gB
�B
1B
FB
 �B
�B
dB
�B
B
�B
�B
�B
�B
)B
EB
;B
'8B
-�B
3�B
2�B
/�B
.IB
-)B
,WB
+QB
*0B
'RB
!�B
#B
B
{B
�B
2B
�B
QB
7B
5B
"hB
'B
.�B
49B
>�B
>�B
AoB
H�B
I�B
J�B
K�B
MB
O\B
P.B
PB
P.B
P�B
P}B
P�B
P.B
P�B
O�B
OB
K�B
H1B
F?B
CB
>]B
<B
9�B
4nB
4B
5%B
6FB
6+B
5�B
33B
0!B
-wB
+QB
(�B
&�B
OB
YB
�B
�B
�B
�B
 B
TB
mB
�B
)B
CB
�B
�B
/B
�B
�B
�B
�B
�B
�B
�B
�B
�B
qB
�B
eB
�B
�B
�B
�B
YB
#B
=B
WB
VB
 �B
!HB
#�B
$tB
"�B
!bB
 vB
�B
�B
5B
IB
B
�B
B
sB
�B
�B
�B
FB
&B
�B
�B
HB
�B
B
�B
B
�B
�B
vB
BB
�B
�B
�B
�B
B
�B
�B
6B
�B
�B
�B
�B
�B
VB
"B
�B
0B
xB
B
�B
B

�B

�B
	�B
�B
fB
�B
�B
�B
�B
�B
9B
�B
_B
�B
�B
�B
?B
�B
�B
aB
GB
aB
�B
B
B
�B
B
�B
�B
aB
�B
GB
B
�B
B
AB
'B
B
-B
�B
gB
B
B
gB
�B
B
 �B
 �B	��B
 iB
  B	��B	��B	��B	��B	��B
 B
�B
�B
 �B
 OB
 B
 4B
 4B
 �B
 �B
 B
B
B
MB
SB
�B
YB
B
YB
%B
�B
mB
mB
?B
B
�B
zB
zB
zB
B
EB
�B
B
�B
�B
�B
�B
�B
�B
�B
B
?B
�B
B
�B
_B
zB
_B
+B
B
B
fB
�B
�B
	lB
	�B
	RB
	RB
	lB

XB

�B

=B

rB
DB
�B
�B
�B
�B
xB
~B
�B
B
PB
�B
VB
pB
�B
B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
,B
{B
�B
�B
gB
�B
�B
�B
�B
SB
?B
�B
�B
B
�B
�B
B
yB
EB
1B
eB
1B
1B
�B
�B
WB
�B
�B
�B
�B
�B
=B
#B
WB
B
CB
�B
�B
~B
�B
B
OB
�B
�B
!B
;B
�B
pB
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!|B
!�B
!�B
"B
"B
"4B
"�B
#B
# B
#nB
#�B
#�B
$@B
$�B
$�B
%`B
%�B
&fB
&�B
&�B
&�B
&�B
'B
'B
'B
'mB
'8B
&�B
&�B
&�B
'B
'mB
'8B
'8B
'�B
'�B
'�B
'�B
(sB
)B
*KB
*B
+6B
+6B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+QB
+B
+B
+B
+QB
+�B
,=B
,�B
,�B
,�B
-wB
-�B
/5B
/iB
/�B
/�B
/�B
0UB
0;B
0�B
0�B
0�B
0�B
1B
1B
1vB
1�B
2B
2B
2aB
2�B
2�B
2�B
2�B
3B
3�B
4B
4B
4B
4TB
4nB
4�B
5�B
6+B
6�B
72B
72B
7B
6�B
6�B
88B
7�B
7�B
9	B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
:*B
:xB
:�B
:�B
:�B
:�B
:�B
;�B
;B
;JB
;�B
<B
<PB
<PB
<�B
<�B
=<B
=VB
=�B
>]B
>�B
?B
?B
?B
?cB
?�B
?}B
?�B
?�B
@�B
A B
A;B
AUB
AUB
A�B
BAB
B�B
B�B
B�B
B�B
B�B
C-B
CaB
CGB
CaB
C�B
DB
D�B
D�B
DgB
D�B
EB
ESB
EmB
E�B
E�B
E�B
F�B
F�B
F�B
GB
GB
GB
G+B
GzB
G�B
G�B
HB
HfB
HfB
H�B
IB
IlB
IlB
I�B
I�B
I�B
I�B
J	B
J#B
J=B
J=B
J=B
JrB
JXB
J=B
JXB
J�B
J�B
KB
K^B
K�B
L0B
L�B
MB
M6B
MPB
MjB
M�B
M�B
M�B
N"B
NVB
N�B
N�B
N�B
N�B
N�B
O(B
OBB
OvB
O\B
OBB
O�B
P.B
O�B
P.B
PHB
PHB
PHB
P}B
P�B
Q B
QB
Q B
Q4B
Q�B
Q�B
RB
R B
RTB
R�B
R�B
R�B
R�B
SB
S@B
S&B
R�B
R�B
S�B
S�B
S�B
T{B
T�B
U�B
UgB
U�B
U�B
U�B
U�B
U�B
VB
V�B
WYB
WYB
WsB
W�B
XEB
X_B
X_B
X�B
X�B
YB
YB
Y1B
Y1B
YeB
YB
Y�B
ZQB
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[qB
[qB
[WB
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]dB
]~B
]~B
^B
^jB
^�B
^�B
^jB
_!B
_!B
_!B
_VB
_VB
_�B
_�B
`B
`'B
`vB
`�B
`�B
`�B
aB
a-B
aHB
aHB
aHB
a|B
abB
abB
a�B
bB
a�B
b4B
bNB
b�B
c B
c�B
c�B
c�B
dZB
dZB
d�B
d�B
d�B
d�B
e,B
e,B
eB
e`B
ezB
e�B
e�B
fB
fB
fLB
fLB
ffB
f�B
gB
gB
gB
gB
gB
g8B
g8B
g8B
g�B
g�B
h
B
h$B
h>B
h>B
h>B
h�B
h�B
h�B
iDB
i*B
i�B
i�B
jKB
jeB
jKB
j0B
j�B
k�B
k�B
lB
l"B
l=B
l�B
l�B
l�B
l�B
mB
m)B
m)B
mCB
m]B
mwB
m�B
m�B
nB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
oB
oOB
o�B
o�B
o�B
pB
pUB
p;B
p�B
p�B
p�B
qB
qAB
q�B
q�B
r-B
rGB
r|B
r�B
r�B
sB
r�B
s3B
s�B
s�B
t9B
tnB
tnB
t�B
t�B
t�B
t�B
u%B
u�B
vB
v+B
v`B
vzB
v`B
v�B
v�B
v�B
v�B
wfB
wfB
w�B
w�B
xRB
xRB
xlB
xRB
xlB
x�B
x�B
y	B
y	B
y	B
y>B
y>B
yXB
y�B
y�B
zDB
z�B
z�B
{B
{0B
{JB
{JB
{dB
{JB
{�B
{�B
{�B
|6B
|P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105248  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192740  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192740  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192740                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042748  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042748  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                