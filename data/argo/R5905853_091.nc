CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-09T00:18:11Z creation;2022-06-09T00:18:12Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220609001811  20220609040306  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               [A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @و�n,��1   @و�ޠ#@.D���S��cT�n��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�33B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�33B���B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CFL�CH  CJ  CL  CM�fCP  CQ�fCS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp33Cq��Ct�Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�C3Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @ ��@s�@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B�B�k�B���B���B���B���B���B���B���B���B���B���B���B���B�8RB���B�B�k�B���B���C��C��C��C��C	��C��C��C��C��C��C�C�C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CD�CF5�CG��CI��CK��CM�\CO��CQ�\CS�\CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Cp)Cq��Ct�Cu�\Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D �Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�@RD�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ʌA��EA��[AпHAм�Aз�Aе?Aи�AЬ�AР\AМ�AМ�AП!AИ�AИ�AИ�AМAК�AЕ�AЖSAЗ�AИ_AЗ�AЖ�AЗ�AМxAНAН~AН�AП�AР�AТhAПVAН�AНIAЛ=AЛ=AЖ�AГ�AГ@AВ:A�~]A�JXAά�Aǟ�A�-CA��A��A�A��wA���A��\A�ݘA��CA�AUA�%A���A��A��A���A�hsA��vA���A�;�A���A��A��,A�u�A��A�v�A���A�7LA���A��oA�YA�\]A���A���A�d�A��A�'�A�9�A�oiA�^�A�sA�^jA�E�A��#A�ZQA�JA���A�B�A��A��dA���A��A�ŢA���A��A��A��	A}p;Ax��As/�Ap6�Ah�Ac��A`�KA^�A\%�AY5�AW�'AVG�AU�AS�QAN�AJ��AG�mAF\�AEf�ADp�A>�A:��A4u�A2��A.��A,,=A+�sA+��A+xA*�A)0�A)1'A(ĜA(A'v�A'�<A)5�A)�\A)�A'�A'dZA'C�A&��A%�dA$p;A#�rA#Z�A"�oA"ѷA"�"A"�A!ߤA!��A (�AQA)�A�MA�QA iAd�A�A�A�~A>BA1A��A֡A��As�AP�A iA��AMA��A�}A
�A��A#:A��A�bA��AMA2aA	Ae,A�vA/�A_A��A��Ak�A�KA��A�"AbA�aA��A
��A
A	�A	�|A	VA	oA�[A�	A�CA%FA� AQ�A�/Au�AT�A��AL�A	A<6AںA1�AMjAOA�ADgA��AR�AMA �;A �A R�A 0UA  �@�1�@�L0@���@��@�w2@��<@�q@��t@�7L@��^@�+�@��U@�K^@�)�@�t�@��6@��@���@�!@��@��@���@�[@��@�l�@��g@�y�@�d�@�˒@�33@� �@譬@�A @扠@���@�!�@��@�-�@��Z@�t�@��@�t@��@��@�˒@�A�@��@�e�@ݪ�@���@�C-@�,=@ە�@�&@���@�p;@٩*@��@؄�@� �@׳�@�=�@�<�@Ք�@��@���@��E@�!�@ӓ�@҂A@ч�@о@Е�@�\�@��@Ϸ@�|�@ϒ:@ϰ�@�S�@���@��m@�V@ͅ@��@��@�!-@��K@̕@�1@ˬq@���@ʾ@�a|@��a@�7L@��@�Ɇ@�+k@ǧ�@ǆ�@��@Ƅ�@�h
@�~@���@œ@�=�@�ߤ@�m�@�#:@á�@�G�@�o@��p@¨�@�@�)�@���@�C�@���@�}V@��@��@��:@��@��p@���@�j@�\�@�@�@�b@��z@�o @��5@���@��Z@���@���@�خ@�{J@�_p@�Q�@��5@��}@�-@��3@��'@�{J@�
=@���@�7�@�U�@��#@���@�Ĝ@��.@��j@�l�@��@�C-@��g@��@�G�@��@��'@�M�@�خ@�zx@�J�@���@��@�Xy@�Ov@��@��@�!-@�C�@��@�	@���@�ƨ@��@���@�	l@��<@��F@�bN@���@���@�s@�A @���@�xl@�_@�1�@�@���@���@��j@��^@�Q�@�_�@��@�l�@�1�@��@���@�6@���@���@�b�@�	l@���@�C-@���@���@�c�@� \@��X@���@�Z�@�@�� @�N<@��z@��>@���@�~�@�s@�n/@�9�@���@���@�z@�v�@��@���@�@�L0@���@�rG@� \@���@��@�_�@�E�@�4n@�4@���@���@��=@��X@��@���@�RT@���@��!@��@���@��s@���@���@���@��D@�p�@�c�@�P�@�;@��@��?@�u�@��+@�~�@�X@�6z@��@��m@�p;@�x@��@���@�T�@���@���@�z�@�+k@���@���@���@��P@��@��[@��@�8�@���@���@���@��@��B@��)@���@�Z@��z@�p�@�T�@�#�@���@���@�p;@�U2@�D�@���@��P@�S&@�5�@�ی@�Xy@���@���@�Y�@��@���@�خ@�x@��@��"@���@���@�i�@�/�@���@���@��V@�n/@���@���@�YK@�/�@��@�@�&@�[@"�@~�1@}��@|w�@|-�@{�m@{S�@z��@zC�@y��@y�-@yhs@y�@x�v@x�@w�$@w8@w@v�"@v��@vYK@u��@ue,@u2a@ttT@t~@t�@t  @s�]@s{J@s
=@r�@r͟@rGE@q�C@q7L@p�U@p��@pM@o�@o!-@n�!@nH�@m�>@mY�@m \@l�D@k��@kJ#@j�@j}V@iT�@h�@h�@h��@h�@htT@hN�@h	�@g��@g+@ezx@e�@dی@d�u@dw�@d(�@c�K@cƨ@cX�@c@bv�@b=q@a�@a��@ao @a%F@`��@_��@_�@^��@^ff@]��@]�@\ی@\��@\A�@[�;@[j�@Z��@Z��@Z0U@Y�@Y*0@X��@W��@W]�@V�B@VC�@U�d@U-w@T�[@T��@Ty>@TH@T~@S�&@S��@Sx@Sj�@R�@R@QrG@Q�@P~(@O�Q@OC@N��@NO@Mu�@L��@L��@Lb@Kg�@J��@JQ@I�#@I��@IG�@H�@H��@H<�@H@G�+@G�Q@G�@G�:@G)_@F�@FC�@F�@Ec@E@D1'@C��@C�@CC�@C�@B��@Bp;@B^5@B@�@B$�@B	@A��@A�'@A?}@@�@@oi@?�F@?iD@?O@?4�@?�@>�b@>kQ@>&�@=��@=}�@=\�@=A @=;@<�?@<��@<e�@<H@<1'@<@;�a@;�@;��@;iD@;�@:��@:Ov@9�o@9�-@9�S@98�@8��@8[�@81@7�@7˒@7�q@7e�@6�@6{�@6�@5��@5�X@5�h@5A @4�v@4V�@47@3�@3��@3s@3e�@3K�@31�@2��@2��@1�@1�S@1m]@1Vm@1@0��@0@/�@/Mj@.�+@.Q@.-@-�@-��@-��@-zx@-8�@,�@,m�@,PH@,-�@,6@,�@+�@+��@+�V@+��@+X�@+@*�}@)��@)7L@);@(�@(U2@(x@'��@'|�@&ߤ@&��@&��@&n�@&5?@&�@&
�@%��@%�>@%��@%�7@%#�@$�p@$1@#a@#6z@#,�@"�c@"�2@"҉@"�A@"Q@"B[@"H�@"R�@"z@"��@"YK@!�@!�S@!S&@!<6@!�@ ��@ tT@ Ft@��@�g@˒@�@C�@�@��@��@q�@?@��@�=@Vm@�	@��@�@w�@V�@'R@��@�m@�&@��@��@�*@o�@9�@
=@�@��@�r@V@?@($@	@�X@�@ی@��@�z@�@N�@4n@��@a@
=@�L@z@s�@s�@R�@+k@��@�#@��@�H@�^@��@��@F@-w@	l@�@Ĝ@�j@��@:�@��@�@H�@
=@��@��@\�@�@ �@�T@�X@c@[W@0�@q@�@ѷ@��@��@_@b@�@�*@��@��@�P@9�@�@�@�<@n�@($@
�@�@�3@@�7@Vm@V@��@oi@H@"h@G@�q@n/@P�@6z@)_@
��@
��@
�@
xl@
kQ@
;�@	�.@	�@	�T@	��@	w2@	-w@�f@��@��@w�@r�@h�@U2@Ft@,=@�@��@v`@j�@W?@A�@;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ʌA��EA��[AпHAм�Aз�Aе?Aи�AЬ�AР\AМ�AМ�AП!AИ�AИ�AИ�AМAК�AЕ�AЖSAЗ�AИ_AЗ�AЖ�AЗ�AМxAНAН~AН�AП�AР�AТhAПVAН�AНIAЛ=AЛ=AЖ�AГ�AГ@AВ:A�~]A�JXAά�Aǟ�A�-CA��A��A�A��wA���A��\A�ݘA��CA�AUA�%A���A��A��A���A�hsA��vA���A�;�A���A��A��,A�u�A��A�v�A���A�7LA���A��oA�YA�\]A���A���A�d�A��A�'�A�9�A�oiA�^�A�sA�^jA�E�A��#A�ZQA�JA���A�B�A��A��dA���A��A�ŢA���A��A��A��	A}p;Ax��As/�Ap6�Ah�Ac��A`�KA^�A\%�AY5�AW�'AVG�AU�AS�QAN�AJ��AG�mAF\�AEf�ADp�A>�A:��A4u�A2��A.��A,,=A+�sA+��A+xA*�A)0�A)1'A(ĜA(A'v�A'�<A)5�A)�\A)�A'�A'dZA'C�A&��A%�dA$p;A#�rA#Z�A"�oA"ѷA"�"A"�A!ߤA!��A (�AQA)�A�MA�QA iAd�A�A�A�~A>BA1A��A֡A��As�AP�A iA��AMA��A�}A
�A��A#:A��A�bA��AMA2aA	Ae,A�vA/�A_A��A��Ak�A�KA��A�"AbA�aA��A
��A
A	�A	�|A	VA	oA�[A�	A�CA%FA� AQ�A�/Au�AT�A��AL�A	A<6AںA1�AMjAOA�ADgA��AR�AMA �;A �A R�A 0UA  �@�1�@�L0@���@��@�w2@��<@�q@��t@�7L@��^@�+�@��U@�K^@�)�@�t�@��6@��@���@�!@��@��@���@�[@��@�l�@��g@�y�@�d�@�˒@�33@� �@譬@�A @扠@���@�!�@��@�-�@��Z@�t�@��@�t@��@��@�˒@�A�@��@�e�@ݪ�@���@�C-@�,=@ە�@�&@���@�p;@٩*@��@؄�@� �@׳�@�=�@�<�@Ք�@��@���@��E@�!�@ӓ�@҂A@ч�@о@Е�@�\�@��@Ϸ@�|�@ϒ:@ϰ�@�S�@���@��m@�V@ͅ@��@��@�!-@��K@̕@�1@ˬq@���@ʾ@�a|@��a@�7L@��@�Ɇ@�+k@ǧ�@ǆ�@��@Ƅ�@�h
@�~@���@œ@�=�@�ߤ@�m�@�#:@á�@�G�@�o@��p@¨�@�@�)�@���@�C�@���@�}V@��@��@��:@��@��p@���@�j@�\�@�@�@�b@��z@�o @��5@���@��Z@���@���@�خ@�{J@�_p@�Q�@��5@��}@�-@��3@��'@�{J@�
=@���@�7�@�U�@��#@���@�Ĝ@��.@��j@�l�@��@�C-@��g@��@�G�@��@��'@�M�@�خ@�zx@�J�@���@��@�Xy@�Ov@��@��@�!-@�C�@��@�	@���@�ƨ@��@���@�	l@��<@��F@�bN@���@���@�s@�A @���@�xl@�_@�1�@�@���@���@��j@��^@�Q�@�_�@��@�l�@�1�@��@���@�6@���@���@�b�@�	l@���@�C-@���@���@�c�@� \@��X@���@�Z�@�@�� @�N<@��z@��>@���@�~�@�s@�n/@�9�@���@���@�z@�v�@��@���@�@�L0@���@�rG@� \@���@��@�_�@�E�@�4n@�4@���@���@��=@��X@��@���@�RT@���@��!@��@���@��s@���@���@���@��D@�p�@�c�@�P�@�;@��@��?@�u�@��+@�~�@�X@�6z@��@��m@�p;@�x@��@���@�T�@���@���@�z�@�+k@���@���@���@��P@��@��[@��@�8�@���@���@���@��@��B@��)@���@�Z@��z@�p�@�T�@�#�@���@���@�p;@�U2@�D�@���@��P@�S&@�5�@�ی@�Xy@���@���@�Y�@��@���@�خ@�x@��@��"@���@���@�i�@�/�@���@���@��V@�n/@���@���@�YK@�/�@��@�@�&@�[@"�@~�1@}��@|w�@|-�@{�m@{S�@z��@zC�@y��@y�-@yhs@y�@x�v@x�@w�$@w8@w@v�"@v��@vYK@u��@ue,@u2a@ttT@t~@t�@t  @s�]@s{J@s
=@r�@r͟@rGE@q�C@q7L@p�U@p��@pM@o�@o!-@n�!@nH�@m�>@mY�@m \@l�D@k��@kJ#@j�@j}V@iT�@h�@h�@h��@h�@htT@hN�@h	�@g��@g+@ezx@e�@dی@d�u@dw�@d(�@c�K@cƨ@cX�@c@bv�@b=q@a�@a��@ao @a%F@`��@_��@_�@^��@^ff@]��@]�@\ی@\��@\A�@[�;@[j�@Z��@Z��@Z0U@Y�@Y*0@X��@W��@W]�@V�B@VC�@U�d@U-w@T�[@T��@Ty>@TH@T~@S�&@S��@Sx@Sj�@R�@R@QrG@Q�@P~(@O�Q@OC@N��@NO@Mu�@L��@L��@Lb@Kg�@J��@JQ@I�#@I��@IG�@H�@H��@H<�@H@G�+@G�Q@G�@G�:@G)_@F�@FC�@F�@Ec@E@D1'@C��@C�@CC�@C�@B��@Bp;@B^5@B@�@B$�@B	@A��@A�'@A?}@@�@@oi@?�F@?iD@?O@?4�@?�@>�b@>kQ@>&�@=��@=}�@=\�@=A @=;@<�?@<��@<e�@<H@<1'@<@;�a@;�@;��@;iD@;�@:��@:Ov@9�o@9�-@9�S@98�@8��@8[�@81@7�@7˒@7�q@7e�@6�@6{�@6�@5��@5�X@5�h@5A @4�v@4V�@47@3�@3��@3s@3e�@3K�@31�@2��@2��@1�@1�S@1m]@1Vm@1@0��@0@/�@/Mj@.�+@.Q@.-@-�@-��@-��@-zx@-8�@,�@,m�@,PH@,-�@,6@,�@+�@+��@+�V@+��@+X�@+@*�}@)��@)7L@);@(�@(U2@(x@'��@'|�@&ߤ@&��@&��@&n�@&5?@&�@&
�@%��@%�>@%��@%�7@%#�@$�p@$1@#a@#6z@#,�@"�c@"�2@"҉@"�A@"Q@"B[@"H�@"R�@"z@"��@"YK@!�@!�S@!S&@!<6@!�@ ��@ tT@ Ft@��@�g@˒@�@C�@�@��@��@q�@?@��@�=@Vm@�	@��@�@w�@V�@'R@��@�m@�&@��@��@�*@o�@9�@
=@�@��@�r@V@?@($@	@�X@�@ی@��@�z@�@N�@4n@��@a@
=@�L@z@s�@s�@R�@+k@��@�#@��@�H@�^@��@��@F@-w@	l@�@Ĝ@�j@��@:�@��@�@H�@
=@��@��@\�@�@ �@�T@�X@c@[W@0�@q@�@ѷ@��@��@_@b@�@�*@��@��@�P@9�@�@�@�<@n�@($@
�@�@�3@@�7@Vm@V@��@oi@H@"h@G@�q@n/@P�@6z@)_@
��@
��@
�@
xl@
kQ@
;�@	�.@	�@	�T@	��@	w2@	-w@�f@��@��@w�@r�@h�@U2@Ft@,=@�@��@v`@j�@W?@A�@;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BgB3BMB3B�BB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B
��B
�fB
�B
�B
�)B
�zB
��B
��BB�BO�BcTB�:B�dB�B�$B�B�EBөB��B��B�`B��B{B
	B �B;BB�B��BoBjB�B B
�B3B�B�B�B�qB�zB��B�B��B�_B�zBcBzBU�B>B%�B B
�B
�qB
�B
�DB
�.B
yXB
dB
B�B

�B	�B	��B	�B	�-B	h$B	XB	K�B	>wB	/�B	'B	B	$B	B	GB��B�B�B��B��B�-B��B�JB�B��BʌB�aB��B�?B	�B	B	#B	.cB	4TB	7fB	=�B	]�B	p�B	�4B	�#B	��B	�_B	��B	�B	��B	��B	��B	��B	�_B	��B	��B	�B	��B	�^B	��B	��B	�?B	�aB	�hB	��B	�>B	�	B	��B	��B	��B	�DB	�B	��B	�dB	��B	��B	�BB	��B	�EB	��B	��B	�JB	̳B	��B	�B	��B	��B	ĶB	�MB	��B	��B	�*B	�6B	��B	�jB	�qB	�B	�4B	�B	�UB	�[B	�GB	��B	�GB	��B	�B	żB	�9B	��B	��B	�(B	�B	��B	�B	�DB	�.B	� B	��B	�B	�jB	�OB	�iB	�"B	� B	ðB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	żB	ȴB	��B	��B	��B	ɠB	��B	ˬB	�DB	�0B	͟B	ΊB	��B	�VB	�pB	�vB	��B	οB	�jB	͟B	�"B	�B	͟B	͹B	͹B	��B	��B	�jB	�PB	�"B	ΥB	�<B	ΊB	ΊB	�<B	�"B	ΊB	�}B	ЗB	� B	�.B	ЗB	�:B	��B	�B	��B	��B	�9B	��B	�_B	��B	��B	�B	��B	ٴB	�B	�eB	�KB	�KB	�KB	�B	�1B	��B	��B	�#B	��B	�7B	׍B	�mB	��B	�FB	�2B	�SB	՛B	�9B	خB	�kB	�B	��B	خB	�B	��B	�/B	ߊB	�|B	�:B	��B	�B	��B	��B	�B	�&B	�B	�B	�,B	�LB	�B	�B	��B	��B	�B	��B	��B	�eB	�6B	��B	�B	�B	��B	�WB	�B	�B	�B	�B	��B	�B	�B	��B	�wB	��B	��B	�B	�wB	��B	��B	��B	�GB	�B	�B	��B	�B	��B	��B	�9B	�B	��B	�tB	��B	�B	�ZB	��B	�	B	��B	�*B	�B	��B	�XB	��B	��B	��B	��B	�B	�B	�GB	�B	�GB	�B	��B	��B	��B	�tB	��B	��B	�fB	��B	��B	�RB	�RB	�XB	��B	�dB	��B	��B	�B	��B	��B	�jB	�qB	�.B	��B
�B
�B
�B
�B
-B
B
�B
�B
�B
YB
%B
tB
�B
B
�B
�B
B
�B
�B
�B
MB
AB
'B
�B
AB
B
B
'B
�B
B
�B
-B
�B
�B
�B
MB
�B
�B
�B
B
�B
gB
�B
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

XB
B
�B
�B
�B
�B
�B
B
�B
�B
�B
	lB

�B

rB
B
B
�B
4B
�B
�B
B
B
.B
�B
B
�B

B
�B
+B
�B
B
	B
xB
]B
~B
dB
B
dB
�B
�B
5B
�B
VB
�B
 �B
 �B
!-B
!�B
!�B
!�B
"4B
"�B
!�B
"hB
"�B
#B
#B
"�B
#�B
#�B
$@B
$tB
$�B
$�B
$@B
$�B
&LB
&�B
&fB
&fB
&�B
&�B
'8B
&�B
'B
&�B
'�B
'�B
'�B
'�B
($B
(XB
(XB
(�B
(�B
(�B
)*B
)yB
*0B
+B
*B
*B
*�B
*B
*�B
+�B
*�B
+QB
+B
./B
-]B
.cB
.IB
.B
.�B
./B
.B
.cB
/OB
0B
0UB
0�B
2-B
2aB
2-B
2|B
2�B
2|B
2GB
3B
2�B
2|B
3MB
4TB
4B
4�B
5�B
4�B
4TB
4TB
5B
5�B
6�B
6`B
6�B
6B
7fB
8B
88B
8�B
:B
9�B
:B
:�B
:*B
9�B
:^B
;JB
;�B
;�B
<6B
<B
<B
<�B
=qB
<�B
<jB
=B
>]B
=VB
=�B
>B
="B
=qB
=�B
>(B
>B
>�B
?.B
>wB
>�B
?�B
@B
@ B
AB
@�B
BAB
BB
B�B
BuB
C-B
C-B
CGB
CaB
C�B
DMB
D�B
D�B
D�B
E�B
FB
FB
FB
FYB
FtB
F�B
F�B
G+B
F�B
F�B
G�B
GEB
HfB
H�B
H�B
I7B
IlB
J#B
JXB
JrB
JrB
KB
K)B
K�B
K�B
K�B
K^B
L0B
L�B
M�B
MB
NB
N<B
OB
O(B
O�B
O�B
P}B
PbB
P�B
Q4B
Q�B
Q�B
R B
R B
RoB
R�B
R�B
S[B
S@B
S[B
S�B
S�B
S�B
S�B
TB
TaB
T�B
T�B
T�B
U�B
V9B
V9B
V9B
V�B
V�B
W
B
W
B
W?B
W$B
W
B
W�B
WYB
W�B
W�B
XyB
X�B
X�B
YB
Y1B
YKB
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[	B
[qB
[qB
[�B
[WB
[�B
[�B
\B
[�B
\B
\)B
\]B
\xB
\�B
]IB
]~B
]�B
]�B
^B
^�B
^�B
^�B
^�B
^�B
_!B
_�B
_;B
_�B
_�B
_�B
`B
`B
`BB
`BB
`�B
aHB
aHB
aHB
`�B
abB
a�B
abB
aHB
bB
a�B
a�B
a|B
abB
a�B
bNB
aB
a�B
a�B
a-B
a-B
aHB
a�B
a�B
a�B
bB
b�B
cB
cTB
c�B
dZB
d�B
d�B
d�B
e�B
e�B
e�B
e�B
fLB
gB
g8B
h>B
hXB
h�B
i*B
iB
i�B
iB
hsB
h�B
h�B
h�B
jKB
jeB
kB
kB
k6B
kkB
k�B
k�B
kB
iyB
i�B
iB
iyB
jB
j�B
j�B
jKB
kB
j�B
k�B
lqB
mCB
m�B
n�B
n�B
oB
n�B
o5B
p!B
pB
pUB
pUB
qB
p�B
p�B
qvB
qB
qAB
q�B
q�B
q[B
q�B
q�B
q�B
r-B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s3B
shB
s�B
s�B
tTB
t9B
t9B
s�B
tnB
tTB
t�B
t�B
t�B
u�B
u�B
vB
u�B
uZB
v+B
u�B
u�B
v�B
v�B
wB
wLB
wfB
wLB
w�B
wfB
w�B
w�B
w�B
w�B
wLB
wfB
w�B
x�B
xB
x�B
x�B
y$B
x�B
x�B
y	B
y�B
y�B
z*B
z�B
zDB
z�B
z�B
{0B
{JB
{�B
{�B
{B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
~(B
}�B
}�B
}qB
~B
~(B
~BB
~�B
.B
~�B
~�B
}B
�B
.B
HB
�B
�OB
��B
��B
��B
��B
��B
�;B
��B
�UB
��B
�[B
�uB
�B
�[B
�'B
�B
��B
�-B
�-B
��B
��B
�{B
�3B
��B
��B
�gB
�B
��B
�B
�9B
�9B
��B
��B
�mB
��B
�SB
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BgB3BMB3B�BB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B
��B
�fB
�B
�B
�)B
�zB
��B
��BB�BO�BcTB�:B�dB�B�$B�B�EBөB��B��B�`B��B{B
	B �B;BB�B��BoBjB�B B
�B3B�B�B�B�qB�zB��B�B��B�_B�zBcBzBU�B>B%�B B
�B
�qB
�B
�DB
�.B
yXB
dB
B�B

�B	�B	��B	�B	�-B	h$B	XB	K�B	>wB	/�B	'B	B	$B	B	GB��B�B�B��B��B�-B��B�JB�B��BʌB�aB��B�?B	�B	B	#B	.cB	4TB	7fB	=�B	]�B	p�B	�4B	�#B	��B	�_B	��B	�B	��B	��B	��B	��B	�_B	��B	��B	�B	��B	�^B	��B	��B	�?B	�aB	�hB	��B	�>B	�	B	��B	��B	��B	�DB	�B	��B	�dB	��B	��B	�BB	��B	�EB	��B	��B	�JB	̳B	��B	�B	��B	��B	ĶB	�MB	��B	��B	�*B	�6B	��B	�jB	�qB	�B	�4B	�B	�UB	�[B	�GB	��B	�GB	��B	�B	żB	�9B	��B	��B	�(B	�B	��B	�B	�DB	�.B	� B	��B	�B	�jB	�OB	�iB	�"B	� B	ðB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	żB	ȴB	��B	��B	��B	ɠB	��B	ˬB	�DB	�0B	͟B	ΊB	��B	�VB	�pB	�vB	��B	οB	�jB	͟B	�"B	�B	͟B	͹B	͹B	��B	��B	�jB	�PB	�"B	ΥB	�<B	ΊB	ΊB	�<B	�"B	ΊB	�}B	ЗB	� B	�.B	ЗB	�:B	��B	�B	��B	��B	�9B	��B	�_B	��B	��B	�B	��B	ٴB	�B	�eB	�KB	�KB	�KB	�B	�1B	��B	��B	�#B	��B	�7B	׍B	�mB	��B	�FB	�2B	�SB	՛B	�9B	خB	�kB	�B	��B	خB	�B	��B	�/B	ߊB	�|B	�:B	��B	�B	��B	��B	�B	�&B	�B	�B	�,B	�LB	�B	�B	��B	��B	�B	��B	��B	�eB	�6B	��B	�B	�B	��B	�WB	�B	�B	�B	�B	��B	�B	�B	��B	�wB	��B	��B	�B	�wB	��B	��B	��B	�GB	�B	�B	��B	�B	��B	��B	�9B	�B	��B	�tB	��B	�B	�ZB	��B	�	B	��B	�*B	�B	��B	�XB	��B	��B	��B	��B	�B	�B	�GB	�B	�GB	�B	��B	��B	��B	�tB	��B	��B	�fB	��B	��B	�RB	�RB	�XB	��B	�dB	��B	��B	�B	��B	��B	�jB	�qB	�.B	��B
�B
�B
�B
�B
-B
B
�B
�B
�B
YB
%B
tB
�B
B
�B
�B
B
�B
�B
�B
MB
AB
'B
�B
AB
B
B
'B
�B
B
�B
-B
�B
�B
�B
MB
�B
�B
�B
B
�B
gB
�B
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

XB
B
�B
�B
�B
�B
�B
B
�B
�B
�B
	lB

�B

rB
B
B
�B
4B
�B
�B
B
B
.B
�B
B
�B

B
�B
+B
�B
B
	B
xB
]B
~B
dB
B
dB
�B
�B
5B
�B
VB
�B
 �B
 �B
!-B
!�B
!�B
!�B
"4B
"�B
!�B
"hB
"�B
#B
#B
"�B
#�B
#�B
$@B
$tB
$�B
$�B
$@B
$�B
&LB
&�B
&fB
&fB
&�B
&�B
'8B
&�B
'B
&�B
'�B
'�B
'�B
'�B
($B
(XB
(XB
(�B
(�B
(�B
)*B
)yB
*0B
+B
*B
*B
*�B
*B
*�B
+�B
*�B
+QB
+B
./B
-]B
.cB
.IB
.B
.�B
./B
.B
.cB
/OB
0B
0UB
0�B
2-B
2aB
2-B
2|B
2�B
2|B
2GB
3B
2�B
2|B
3MB
4TB
4B
4�B
5�B
4�B
4TB
4TB
5B
5�B
6�B
6`B
6�B
6B
7fB
8B
88B
8�B
:B
9�B
:B
:�B
:*B
9�B
:^B
;JB
;�B
;�B
<6B
<B
<B
<�B
=qB
<�B
<jB
=B
>]B
=VB
=�B
>B
="B
=qB
=�B
>(B
>B
>�B
?.B
>wB
>�B
?�B
@B
@ B
AB
@�B
BAB
BB
B�B
BuB
C-B
C-B
CGB
CaB
C�B
DMB
D�B
D�B
D�B
E�B
FB
FB
FB
FYB
FtB
F�B
F�B
G+B
F�B
F�B
G�B
GEB
HfB
H�B
H�B
I7B
IlB
J#B
JXB
JrB
JrB
KB
K)B
K�B
K�B
K�B
K^B
L0B
L�B
M�B
MB
NB
N<B
OB
O(B
O�B
O�B
P}B
PbB
P�B
Q4B
Q�B
Q�B
R B
R B
RoB
R�B
R�B
S[B
S@B
S[B
S�B
S�B
S�B
S�B
TB
TaB
T�B
T�B
T�B
U�B
V9B
V9B
V9B
V�B
V�B
W
B
W
B
W?B
W$B
W
B
W�B
WYB
W�B
W�B
XyB
X�B
X�B
YB
Y1B
YKB
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[	B
[qB
[qB
[�B
[WB
[�B
[�B
\B
[�B
\B
\)B
\]B
\xB
\�B
]IB
]~B
]�B
]�B
^B
^�B
^�B
^�B
^�B
^�B
_!B
_�B
_;B
_�B
_�B
_�B
`B
`B
`BB
`BB
`�B
aHB
aHB
aHB
`�B
abB
a�B
abB
aHB
bB
a�B
a�B
a|B
abB
a�B
bNB
aB
a�B
a�B
a-B
a-B
aHB
a�B
a�B
a�B
bB
b�B
cB
cTB
c�B
dZB
d�B
d�B
d�B
e�B
e�B
e�B
e�B
fLB
gB
g8B
h>B
hXB
h�B
i*B
iB
i�B
iB
hsB
h�B
h�B
h�B
jKB
jeB
kB
kB
k6B
kkB
k�B
k�B
kB
iyB
i�B
iB
iyB
jB
j�B
j�B
jKB
kB
j�B
k�B
lqB
mCB
m�B
n�B
n�B
oB
n�B
o5B
p!B
pB
pUB
pUB
qB
p�B
p�B
qvB
qB
qAB
q�B
q�B
q[B
q�B
q�B
q�B
r-B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s3B
shB
s�B
s�B
tTB
t9B
t9B
s�B
tnB
tTB
t�B
t�B
t�B
u�B
u�B
vB
u�B
uZB
v+B
u�B
u�B
v�B
v�B
wB
wLB
wfB
wLB
w�B
wfB
w�B
w�B
w�B
w�B
wLB
wfB
w�B
x�B
xB
x�B
x�B
y$B
x�B
x�B
y	B
y�B
y�B
z*B
z�B
zDB
z�B
z�B
{0B
{JB
{�B
{�B
{B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
~(B
}�B
}�B
}qB
~B
~(B
~BB
~�B
.B
~�B
~�B
}B
�B
.B
HB
�B
�OB
��B
��B
��B
��B
��B
�;B
��B
�UB
��B
�[B
�uB
�B
�[B
�'B
�B
��B
�-B
�-B
��B
��B
�{B
�3B
��B
��B
�gB
�B
��B
�B
�9B
�9B
��B
��B
�mB
��B
�SB
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220609001348  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220609001811  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220609001812  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220609001812                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220609091817  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220609091817  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220609040306                      G�O�G�O�G�O�                