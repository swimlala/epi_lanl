CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-09T18:41:17Z creation;2022-12-09T18:41:18Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20221209184117  20221209185759  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�#�g(�1   @�$Ib��@/p��
=q�c8bM��1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A��A   AA��A`  A�  A�  A���A�  A�  A�33A�33A�  A�33B  B  B  B   B(  B0ffB8  B@��BG��BO��BX  B`  Bh  Bp  BxffB�  B���B�  B�  B�  B�  B�33B���B�ffB���B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>ffC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ D�|�Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�3D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�Q�@��A (�A�\A@(�A^�\A~�\A�G�A�{A�G�A�G�A�z�A�z�A�G�A�z�B��B��B��B��B'��B0
=B7��B@p�BG=qBO=qBW��B_��Bg��Bo��Bx
=B��B���B���B���B���B���B�B�k�B�8RB���B���B���B���B���B�k�B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B�8RB���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C�\C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C>O\C?�\CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C�HC��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#��D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D½D��D�=D�y�Dù�D��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD� RD�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��RD��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D��RD��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�\]A�^5A�[#A�T,A�V9A�W?A�Y�A�[WA�\]A�b�A�_�A�_�A�Y�A�cTA�l�A�j�A�ncA�xA�~�AӀ4AӂAӃAӃ�Aӄ�AӆYAӇ�AӉ7Aӊ�AӌJAӊ�AӋDA�}�A�T�A��A҉�A��A�5A�YA��VA�\�A��A��A�:�A�W�A���A��A��?A�{�A��[A�{JA���A�H�A�1'A�/�A��'A�`A��)A�=�A�A�A��A�n�A�	�A��EA��A�'�A�ѷA�#�A�HKA���A~�oAy�EAsYKAk��Ah�AfSAd?AaA\��AY�AX�hAW�6AU��AR�XAL�tAII�AG��AE��AC�A>	A;{�A:�/A:}VA8��A4�zA3�}A3!-A2�AA1��A0[�A/��A/��A/��A/�VA.��A-�A-K^A,�#A-A-y�A-U�A- �A,֡A,��A,�+A-�A,��A,��A,�9A,X�A,A,8A+��A+u�A+6A*��A*W?A)یA){JA(C�A'd�A'A&�A&�A&�kA&Z�A%[WA$��A$�kA#n/A"8�A �!A �rA ںA!xA U�A�7A��AĜA�$A��A�"A1�A�AY�A�AQA�fA��AJ�A��AL0A�oA�OAt�A<�A�A��A�PA^�AA AYA�[A8A�kAm]AD�A&A�]A�A��A_pA-A�>A��A��AN�A�cA��AN�A�A�vA��AhsASA��A��A�3A��A:�A��AoiA]dA4A�CA�A�DA;dA�AV�A*�A�A�"A��AW?AOAȴA��A8�A�AA��A�9A��A`BAFtA
j�A	u�A	�A~�A�Aw�A�Au�A��AA��AH�A��A6Ay�A�]A�jA/�A 4�@�@��2A /�A -w@��/@���@�C@�E9@��V@���@���@�J�@��@���@��m@��3@�K�@�<6@��@�&�@�(@��@��H@��@���@�&�@�zx@��@�s@�@��@�1@��+@��@��@��@�F@�!�@��@��[@� @��)@��@�C�@�
=@沖@�W�@��B@�@�@�+k@㽥@�p;@�^@�j�@�|�@�=�@��@�\�@�8@�Y�@��@�8�@���@�Ft@�O�@׮@�z@��@�u�@Ԝx@ӖS@�W?@�?}@��@�'R@њk@�A�@��B@И_@���@��E@�-�@��3@�-w@̵�@�^5@�˒@��@ʚ@�L0@��@ɚk@Ɂ@��2@�#:@���@�c@�H�@ƹ�@�Xy@�M@�zx@���@�c�@���@öF@�c�@­�@��@���@�ƨ@�@�zx@�33@���@�7@�J@�_@���@��@�q@���@�u%@�"h@���@��S@�j�@�)_@���@�y�@�!�@�Ɇ@��u@���@�m�@�$@��H@�Y@���@��@��1@�:*@��@���@��2@�z�@���@�Mj@�V@��,@�n�@��@���@�:�@��@��@��}@�j@���@���@�g�@��@���@�PH@�@��@��C@�J�@��h@�U2@��@���@�_p@��@��+@�M@��@@�E9@��@��m@���@�#:@��7@�Q�@���@�j@��@��|@�9X@��@��Z@��6@��~@��/@�D�@�ƨ@�S&@��@���@�ݘ@�o@��)@��h@��@�{�@�Z�@�B[@�  @���@�&@��b@�p;@�K^@�)�@�b@�j�@��2@�V@��3@�w2@�;d@��@�|�@�a|@�H@�1'@�e@��@���@�v`@�L�@�=@�4@���@�bN@�YK@�J�@�/�@��@�e�@�/�@��@�L0@��@���@�hs@�;d@��@��[@���@�n�@�c�@�Z�@�9X@��D@��g@��3@���@��X@��S@���@�zx@�m]@�Q�@�K�@�@��,@��x@��@�s�@�6z@���@���@��z@���@�C-@�	@���@��@@�x�@��@���@��o@�@�@��0@�w2@�O�@��@�@�@�!@�G@�˒@��h@�e,@���@�V�@���@�e�@�P�@�8@�@��@�g8@�M@��o@��[@�Vm@�Dg@�(�@��B@��@�u�@�n�@�e�@�PH@�=q@�.�@�($@�b@��#@�y�@�-w@���@��]@��R@�v�@�\�@�<�@�  @��Q@���@�S�@�!�@��P@��K@���@�ff@���@���@��=@�j@�J�@�6z@��@��,@�y>@�c @�-�@�6@�@�:@~�@a@~��@~\�@}�j@|�@|S�@{��@{X�@z�L@z&�@z_@y�j@y�@y=�@y�@x�z@x~(@xU2@w�@w�@v�X@v��@ve@u+�@t�@t1@s��@sX�@s6z@sY@r�@qs�@q \@p�j@o�m@o��@o,�@n��@m��@m+�@l�U@loi@l!@j�8@j$�@i�@i�Z@i��@ik�@h��@h@g��@g�P@gA�@f��@f��@f	@eIR@d��@d~@c�:@cg�@c�@b}V@a`B@`�@`�@`�@`��@`��@`_@_��@_e�@_8@^�R@^B[@]��@]S&@\�K@\j@\6@[��@[�	@[A�@Z�y@Z($@Y�N@Yf�@Y%F@X�v@Xy>@W�@W��@WJ#@V�y@V��@VO@U��@U�S@U^�@T�@TQ�@S��@S9�@R�M@R}V@Q��@P��@PS�@Px@O��@O,�@N�@N�+@N�@M��@M5�@L��@L�K@L��@L`�@K��@K��@K8@J�@J�@J��@J�\@Ju%@JJ�@I�N@I�3@I��@I�"@H��@H]d@H2�@H@Hx@H  @G��@G�m@G�
@G�P@G8@F��@F��@FZ�@FB[@E��@E�n@EJ�@E@D�@D�o@C�@C�@@CP�@C,�@C
=@B�2@B�!@Bp;@BOv@BO@Aϫ@A�n@Ac@A/@A�@@�j@@V�@@/�@?�]@?�P@?=@?$t@?o@>�,@>kQ@>E�@>.�@=��@=�@=��@=f�@=2a@=@<֡@<�u@<`�@<,=@;��@;��@;e�@;33@:�]@:��@:�@:��@:^5@:E�@:)�@9�o@9�t@9��@9p�@9%@8��@8z�@7��@7��@7!-@6�X@6�b@6E�@6�@5��@5s�@5@4�/@4�O@4S�@3�r@3��@3RT@3�@2�@2q�@2	@1ϫ@1�@1�@1f�@0�@0��@0D�@0�@/خ@/�a@/��@/��@/=@.�'@.�r@.M�@.4@-��@,��@,g8@,:�@+�;@+iD@+A�@+)_@*�@*͟@*��@*M�@*+k@*�@)�9@)��@)&�@(�@(��@'�@'��@'P�@' i@&ȴ@&�b@&�@&c @&�@%�-@%k�@$��@$Ɇ@$�j@$��@$2�@#�F@#RT@#C@"�@"��@"J�@"�@!��@!��@!w2@!F@!�@ ��@ "h@�&@�w@e�@�@�@��@c @�@��@��@zx@5�@�@��@g8@x@�f@��@ȴ@�A@H�@�@�-@��@IR@��@��@��@��@�@bN@<�@%�@�@�a@��@��@/�@��@�s@��@\�@($@J@�@zx@B�@@�@�U@�@��@A�@�@�[@o�@33@�@��@�R@��@-@�@��@�^@��@c@p�@G�@��@�I@��@��@u�@c�@:�@�@��@�{@dZ@W?@P�@=@)_@
=@�H@�}@��@�F@n�@{@�.@c@+�@�@�@ی@��@�j@�@y>@bN@I�@Ft@7�@1@ݘ@�a@��@�k@4�@
�111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�\]A�^5A�[#A�T,A�V9A�W?A�Y�A�[WA�\]A�b�A�_�A�_�A�Y�A�cTA�l�A�j�A�ncA�xA�~�AӀ4AӂAӃAӃ�Aӄ�AӆYAӇ�AӉ7Aӊ�AӌJAӊ�AӋDA�}�A�T�A��A҉�A��A�5A�YA��VA�\�A��A��A�:�A�W�A���A��A��?A�{�A��[A�{JA���A�H�A�1'A�/�A��'A�`A��)A�=�A�A�A��A�n�A�	�A��EA��A�'�A�ѷA�#�A�HKA���A~�oAy�EAsYKAk��Ah�AfSAd?AaA\��AY�AX�hAW�6AU��AR�XAL�tAII�AG��AE��AC�A>	A;{�A:�/A:}VA8��A4�zA3�}A3!-A2�AA1��A0[�A/��A/��A/��A/�VA.��A-�A-K^A,�#A-A-y�A-U�A- �A,֡A,��A,�+A-�A,��A,��A,�9A,X�A,A,8A+��A+u�A+6A*��A*W?A)یA){JA(C�A'd�A'A&�A&�A&�kA&Z�A%[WA$��A$�kA#n/A"8�A �!A �rA ںA!xA U�A�7A��AĜA�$A��A�"A1�A�AY�A�AQA�fA��AJ�A��AL0A�oA�OAt�A<�A�A��A�PA^�AA AYA�[A8A�kAm]AD�A&A�]A�A��A_pA-A�>A��A��AN�A�cA��AN�A�A�vA��AhsASA��A��A�3A��A:�A��AoiA]dA4A�CA�A�DA;dA�AV�A*�A�A�"A��AW?AOAȴA��A8�A�AA��A�9A��A`BAFtA
j�A	u�A	�A~�A�Aw�A�Au�A��AA��AH�A��A6Ay�A�]A�jA/�A 4�@�@��2A /�A -w@��/@���@�C@�E9@��V@���@���@�J�@��@���@��m@��3@�K�@�<6@��@�&�@�(@��@��H@��@���@�&�@�zx@��@�s@�@��@�1@��+@��@��@��@�F@�!�@��@��[@� @��)@��@�C�@�
=@沖@�W�@��B@�@�@�+k@㽥@�p;@�^@�j�@�|�@�=�@��@�\�@�8@�Y�@��@�8�@���@�Ft@�O�@׮@�z@��@�u�@Ԝx@ӖS@�W?@�?}@��@�'R@њk@�A�@��B@И_@���@��E@�-�@��3@�-w@̵�@�^5@�˒@��@ʚ@�L0@��@ɚk@Ɂ@��2@�#:@���@�c@�H�@ƹ�@�Xy@�M@�zx@���@�c�@���@öF@�c�@­�@��@���@�ƨ@�@�zx@�33@���@�7@�J@�_@���@��@�q@���@�u%@�"h@���@��S@�j�@�)_@���@�y�@�!�@�Ɇ@��u@���@�m�@�$@��H@�Y@���@��@��1@�:*@��@���@��2@�z�@���@�Mj@�V@��,@�n�@��@���@�:�@��@��@��}@�j@���@���@�g�@��@���@�PH@�@��@��C@�J�@��h@�U2@��@���@�_p@��@��+@�M@��@@�E9@��@��m@���@�#:@��7@�Q�@���@�j@��@��|@�9X@��@��Z@��6@��~@��/@�D�@�ƨ@�S&@��@���@�ݘ@�o@��)@��h@��@�{�@�Z�@�B[@�  @���@�&@��b@�p;@�K^@�)�@�b@�j�@��2@�V@��3@�w2@�;d@��@�|�@�a|@�H@�1'@�e@��@���@�v`@�L�@�=@�4@���@�bN@�YK@�J�@�/�@��@�e�@�/�@��@�L0@��@���@�hs@�;d@��@��[@���@�n�@�c�@�Z�@�9X@��D@��g@��3@���@��X@��S@���@�zx@�m]@�Q�@�K�@�@��,@��x@��@�s�@�6z@���@���@��z@���@�C-@�	@���@��@@�x�@��@���@��o@�@�@��0@�w2@�O�@��@�@�@�!@�G@�˒@��h@�e,@���@�V�@���@�e�@�P�@�8@�@��@�g8@�M@��o@��[@�Vm@�Dg@�(�@��B@��@�u�@�n�@�e�@�PH@�=q@�.�@�($@�b@��#@�y�@�-w@���@��]@��R@�v�@�\�@�<�@�  @��Q@���@�S�@�!�@��P@��K@���@�ff@���@���@��=@�j@�J�@�6z@��@��,@�y>@�c @�-�@�6@�@�:@~�@a@~��@~\�@}�j@|�@|S�@{��@{X�@z�L@z&�@z_@y�j@y�@y=�@y�@x�z@x~(@xU2@w�@w�@v�X@v��@ve@u+�@t�@t1@s��@sX�@s6z@sY@r�@qs�@q \@p�j@o�m@o��@o,�@n��@m��@m+�@l�U@loi@l!@j�8@j$�@i�@i�Z@i��@ik�@h��@h@g��@g�P@gA�@f��@f��@f	@eIR@d��@d~@c�:@cg�@c�@b}V@a`B@`�@`�@`�@`��@`��@`_@_��@_e�@_8@^�R@^B[@]��@]S&@\�K@\j@\6@[��@[�	@[A�@Z�y@Z($@Y�N@Yf�@Y%F@X�v@Xy>@W�@W��@WJ#@V�y@V��@VO@U��@U�S@U^�@T�@TQ�@S��@S9�@R�M@R}V@Q��@P��@PS�@Px@O��@O,�@N�@N�+@N�@M��@M5�@L��@L�K@L��@L`�@K��@K��@K8@J�@J�@J��@J�\@Ju%@JJ�@I�N@I�3@I��@I�"@H��@H]d@H2�@H@Hx@H  @G��@G�m@G�
@G�P@G8@F��@F��@FZ�@FB[@E��@E�n@EJ�@E@D�@D�o@C�@C�@@CP�@C,�@C
=@B�2@B�!@Bp;@BOv@BO@Aϫ@A�n@Ac@A/@A�@@�j@@V�@@/�@?�]@?�P@?=@?$t@?o@>�,@>kQ@>E�@>.�@=��@=�@=��@=f�@=2a@=@<֡@<�u@<`�@<,=@;��@;��@;e�@;33@:�]@:��@:�@:��@:^5@:E�@:)�@9�o@9�t@9��@9p�@9%@8��@8z�@7��@7��@7!-@6�X@6�b@6E�@6�@5��@5s�@5@4�/@4�O@4S�@3�r@3��@3RT@3�@2�@2q�@2	@1ϫ@1�@1�@1f�@0�@0��@0D�@0�@/خ@/�a@/��@/��@/=@.�'@.�r@.M�@.4@-��@,��@,g8@,:�@+�;@+iD@+A�@+)_@*�@*͟@*��@*M�@*+k@*�@)�9@)��@)&�@(�@(��@'�@'��@'P�@' i@&ȴ@&�b@&�@&c @&�@%�-@%k�@$��@$Ɇ@$�j@$��@$2�@#�F@#RT@#C@"�@"��@"J�@"�@!��@!��@!w2@!F@!�@ ��@ "h@�&@�w@e�@�@�@��@c @�@��@��@zx@5�@�@��@g8@x@�f@��@ȴ@�A@H�@�@�-@��@IR@��@��@��@��@�@bN@<�@%�@�@�a@��@��@/�@��@�s@��@\�@($@J@�@zx@B�@@�@�U@�@��@A�@�@�[@o�@33@�@��@�R@��@-@�@��@�^@��@c@p�@G�@��@�I@��@��@u�@c�@:�@�@��@�{@dZ@W?@P�@=@)_@
=@�H@�}@��@�F@n�@{@�.@c@+�@�@�@ی@��@�j@�@y>@bN@I�@Ft@7�@1@ݘ@�a@��@�k@4�@
�111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�B�B��B��B��B��B��B��B��B��B��B��B��B�-B��B��B�3B�B�B��B�tB��B�B��B�B��B��B��B��B��B�B�iB	�B	@B	��B
b�B
wB
{�B
�@B
�zB
�?B �B[B�B�B�B�B�BsB�BVB 4B
�B
��B�B
=B{B
��B
��B
�zB
�B
�0B
�tB
�.B
{B
QhB
#�B	�B	��B	��B	��B	��B	�OB	t�B	j�B	_B	M�B	;�B	-�B	)*B	%�B	�B	.B�GB�6B��B��B��B�B�4B�5B�B�B�B�}B�B�zB�>B��B	�B	gB	-�B	EB	G_B	SB	jeB	vFB	��B	�B	��B	��B	�4B	��B	��B	�B	�`B
B
EB
�B
�B
#:B
)DB
)�B
+�B
-�B
2�B
1�B
0B
0�B
.�B
.�B
.�B
/ B
/�B
0;B
1B
2-B
3B
1'B
+�B
(�B
'mB
8�B
F%B
CGB
D3B
HB
MjB
RoB
U�B
U�B
T�B
R�B
RB
R�B
T�B
T{B
T{B
TaB
TB
S�B
TFB
S�B
T�B
UgB
U�B
W
B
W�B
W�B
W�B
W�B
XyB
XB
W�B
X_B
X�B
X_B
W�B
W?B
V�B
V�B
W?B
V�B
U�B
V9B
VB
U�B
UB
T,B
S�B
S@B
U�B
VmB
T�B
S�B
S[B
R�B
RB
Q B
O�B
O(B
OB
OBB
P�B
N�B
M�B
L�B
LdB
MPB
MB
L�B
LdB
LdB
K�B
J�B
I7B
G�B
E�B
E�B
I�B
O�B
P�B
O�B
NB
K)B
C�B
BuB
AoB
?B
=�B
<B
9�B
4nB
.IB
%�B
�B
YB
4B
�B
B
xB
+B
B
(B
\B
QB
 \B
#B
�B
�B
�B
�B
$tB
'�B
%�B
$�B
#�B
"hB
"�B
!�B
# B
#B
 BB
B
�B
#:B
!�B
;B
B
�B
!|B
"�B
!�B
 �B
�B
_B
�B
vB

�B
SB
[B
�B
aB
�B
uB
�B
�B
�B
YB
+B
�B
�B
�B
�B
	�B
�B
B
	B
�B
(B
�B
�B
"B

rB
�B
)B
	�B
�B
�B
B	�qB	��B	�B	��B	��B	�lB	��B	��B	�rB	��B	�$B	��B	�lB	�LB	�%B	�B	��B	�B	�B	�3B	�B	��B	�aB	��B	��B	�B	��B	�TB	�%B	�%B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�+B	�`B	��B	��B	��B	�2B	�LB	�FB	��B	��B	��B	�FB	�FB	�zB	��B	�2B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B	�B	��B	�qB	��B	��B	��B	��B	�]B	�B	��B	�wB	��B	�.B	��B
  B
 4B
 �B
UB
UB
�B
�B
oB
�B
�B
B
'B
uB
�B
-B
�B
�B
�B
B
�B
MB
B
B
mB
mB
�B
�B
�B
%B
�B
tB
%B
?B
�B
B
B
EB
zB
�B
	B
	�B
	�B
	�B
	�B
	�B

�B

�B
^B
B
~B
B
pB
vB
�B
�B
B
�B
�B
�B
�B
�B
B
MB
�B
�B
MB
mB
YB
�B
�B

B
�B
mB
�B
�B
�B

B
$B
?B
YB
�B
�B
�B
eB
�B
�B
xB
�B
�B
jB
5B
B
�B
�B
xB
�B
B
�B
B
OB
�B
�B
5B
B
B
OB
�B
;B
;B
VB
pB
�B
 B
 BB
 �B
 �B
 �B
!HB
!�B
"4B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"NB
"�B
"�B
"�B
#nB
#�B
#�B
$&B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'RB
($B
)_B
)DB
)_B
)_B
)�B
)�B
*0B
*eB
*�B
+B
+QB
+QB
+QB
,B
,"B
,"B
,=B
,"B
,"B
,B
,B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
./B
.�B
.�B
.�B
/5B
/iB
/�B
0oB
0�B
0�B
0�B
1B
1vB
1�B
1�B
2GB
33B
3MB
3MB
3�B
4B
49B
4�B
4�B
4�B
5?B
5%B
5tB
5ZB
6B
5�B
6zB
6�B
72B
7B
7�B
8B
8�B
8lB
8�B
8�B
9	B
9$B
9$B
9rB
9$B
9�B
9�B
9$B
8�B
8�B
8lB
8�B
8�B
8�B
9XB
9rB
9$B
9�B
9�B
9�B
:*B
;B
;�B
<6B
;�B
;B
;B
:xB
:DB
9�B
9�B
:^B
:�B
:�B
;B
;�B
;�B
;�B
;�B
<B
<B
<�B
<�B
<PB
<�B
="B
=qB
=�B
=�B
=�B
>wB
?�B
?�B
?�B
@ B
?�B
?�B
@ B
@�B
@�B
@�B
A�B
BuB
C{B
C{B
C�B
C�B
DB
D�B
FYB
F�B
F�B
G+B
GEB
G�B
G�B
HKB
H�B
IB
IB
I�B
I�B
J	B
JrB
JrB
JXB
JXB
J�B
KxB
K�B
L0B
LJB
LdB
L�B
MPB
MjB
M�B
NB
N<B
NVB
N�B
N�B
O(B
O�B
O�B
O�B
O�B
PbB
P�B
Q B
QNB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R B
R B
RB
S@B
S&B
S[B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
UMB
U�B
U�B
VB
VB
V�B
W
B
W$B
W�B
W�B
W�B
W�B
W�B
XB
XB
XEB
XyB
XyB
X�B
X�B
X�B
YeB
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
\B
\)B
\CB
\]B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
]�B
]�B
^B
^B
^5B
^�B
^jB
^�B
^�B
_B
_B
_B
_�B
_�B
`B
`vB
`\B
abB
a-B
aHB
a�B
a�B
a�B
b�B
b�B
b�B
b�B
cTB
c�B
c�B
d@B
d@B
dtB
d�B
e,B
e`B
e,B
e`B
e`B
e�B
fB
f�B
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
hXB
h�B
iyB
i_B
i�B
j0B
j0B
jKB
j0B
jB
j�B
kB
kB
kkB
k�B
k�B
l=B
l"B
l�B
m)B
m]B
m�B
m�B
nB
n/B
n/B
ncB
n�B
oB
o5B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
qB
qAB
qvB
q�B
r-B
r-B
r|B
r�B
r�B
r�B
sB
s�B
s�B
s�B
tTB
tnB
t�B
t�B
uB
u?B
uZB
u�B
u�B
u�B
vFB
vFB
vzB
v�B
wLB
w�B
xB
xRB
x�B
y>B
y�B
y�B
y�B
y�B
zDB
zDB
z�B
zxB
z�B
z�B
z�B
z�B
{B
z�B
{0B
{dB
{B
{�B
{�B
|B
|B
|PB
|PB
|�B
|�B
|�B
}B
}VB
}�B
}�B
}�B
~(B
~BB
~wB
~�B
~�B
~�B
~�B
B
�B
�B
}B
�B
�B
� B
�B
�B
��B
��B
��B
��B
��B
��B
� B
�B
�UB
��B
��B
��B
��B
��B
��B
��B
�B
�[B
��B
��B
��B
��B
�GB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�3B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�B�B��B��B��B��B��B��B��B��B��B��B��B�-B��B��B�3B�B�B��B�tB��B�B��B�B��B��B��B��B��B�B�iB	�B	@B	��B
b�B
wB
{�B
�@B
�zB
�?B �B[B�B�B�B�B�BsB�BVB 4B
�B
��B�B
=B{B
��B
��B
�zB
�B
�0B
�tB
�.B
{B
QhB
#�B	�B	��B	��B	��B	��B	�OB	t�B	j�B	_B	M�B	;�B	-�B	)*B	%�B	�B	.B�GB�6B��B��B��B�B�4B�5B�B�B�B�}B�B�zB�>B��B	�B	gB	-�B	EB	G_B	SB	jeB	vFB	��B	�B	��B	��B	�4B	��B	��B	�B	�`B
B
EB
�B
�B
#:B
)DB
)�B
+�B
-�B
2�B
1�B
0B
0�B
.�B
.�B
.�B
/ B
/�B
0;B
1B
2-B
3B
1'B
+�B
(�B
'mB
8�B
F%B
CGB
D3B
HB
MjB
RoB
U�B
U�B
T�B
R�B
RB
R�B
T�B
T{B
T{B
TaB
TB
S�B
TFB
S�B
T�B
UgB
U�B
W
B
W�B
W�B
W�B
W�B
XyB
XB
W�B
X_B
X�B
X_B
W�B
W?B
V�B
V�B
W?B
V�B
U�B
V9B
VB
U�B
UB
T,B
S�B
S@B
U�B
VmB
T�B
S�B
S[B
R�B
RB
Q B
O�B
O(B
OB
OBB
P�B
N�B
M�B
L�B
LdB
MPB
MB
L�B
LdB
LdB
K�B
J�B
I7B
G�B
E�B
E�B
I�B
O�B
P�B
O�B
NB
K)B
C�B
BuB
AoB
?B
=�B
<B
9�B
4nB
.IB
%�B
�B
YB
4B
�B
B
xB
+B
B
(B
\B
QB
 \B
#B
�B
�B
�B
�B
$tB
'�B
%�B
$�B
#�B
"hB
"�B
!�B
# B
#B
 BB
B
�B
#:B
!�B
;B
B
�B
!|B
"�B
!�B
 �B
�B
_B
�B
vB

�B
SB
[B
�B
aB
�B
uB
�B
�B
�B
YB
+B
�B
�B
�B
�B
	�B
�B
B
	B
�B
(B
�B
�B
"B

rB
�B
)B
	�B
�B
�B
B	�qB	��B	�B	��B	��B	�lB	��B	��B	�rB	��B	�$B	��B	�lB	�LB	�%B	�B	��B	�B	�B	�3B	�B	��B	�aB	��B	��B	�B	��B	�TB	�%B	�%B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�+B	�`B	��B	��B	��B	�2B	�LB	�FB	��B	��B	��B	�FB	�FB	�zB	��B	�2B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B	�B	��B	�qB	��B	��B	��B	��B	�]B	�B	��B	�wB	��B	�.B	��B
  B
 4B
 �B
UB
UB
�B
�B
oB
�B
�B
B
'B
uB
�B
-B
�B
�B
�B
B
�B
MB
B
B
mB
mB
�B
�B
�B
%B
�B
tB
%B
?B
�B
B
B
EB
zB
�B
	B
	�B
	�B
	�B
	�B
	�B

�B

�B
^B
B
~B
B
pB
vB
�B
�B
B
�B
�B
�B
�B
�B
B
MB
�B
�B
MB
mB
YB
�B
�B

B
�B
mB
�B
�B
�B

B
$B
?B
YB
�B
�B
�B
eB
�B
�B
xB
�B
�B
jB
5B
B
�B
�B
xB
�B
B
�B
B
OB
�B
�B
5B
B
B
OB
�B
;B
;B
VB
pB
�B
 B
 BB
 �B
 �B
 �B
!HB
!�B
"4B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"NB
"�B
"�B
"�B
#nB
#�B
#�B
$&B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'RB
($B
)_B
)DB
)_B
)_B
)�B
)�B
*0B
*eB
*�B
+B
+QB
+QB
+QB
,B
,"B
,"B
,=B
,"B
,"B
,B
,B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
./B
.�B
.�B
.�B
/5B
/iB
/�B
0oB
0�B
0�B
0�B
1B
1vB
1�B
1�B
2GB
33B
3MB
3MB
3�B
4B
49B
4�B
4�B
4�B
5?B
5%B
5tB
5ZB
6B
5�B
6zB
6�B
72B
7B
7�B
8B
8�B
8lB
8�B
8�B
9	B
9$B
9$B
9rB
9$B
9�B
9�B
9$B
8�B
8�B
8lB
8�B
8�B
8�B
9XB
9rB
9$B
9�B
9�B
9�B
:*B
;B
;�B
<6B
;�B
;B
;B
:xB
:DB
9�B
9�B
:^B
:�B
:�B
;B
;�B
;�B
;�B
;�B
<B
<B
<�B
<�B
<PB
<�B
="B
=qB
=�B
=�B
=�B
>wB
?�B
?�B
?�B
@ B
?�B
?�B
@ B
@�B
@�B
@�B
A�B
BuB
C{B
C{B
C�B
C�B
DB
D�B
FYB
F�B
F�B
G+B
GEB
G�B
G�B
HKB
H�B
IB
IB
I�B
I�B
J	B
JrB
JrB
JXB
JXB
J�B
KxB
K�B
L0B
LJB
LdB
L�B
MPB
MjB
M�B
NB
N<B
NVB
N�B
N�B
O(B
O�B
O�B
O�B
O�B
PbB
P�B
Q B
QNB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R B
R B
RB
S@B
S&B
S[B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
UMB
U�B
U�B
VB
VB
V�B
W
B
W$B
W�B
W�B
W�B
W�B
W�B
XB
XB
XEB
XyB
XyB
X�B
X�B
X�B
YeB
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
\B
\)B
\CB
\]B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
]�B
]�B
^B
^B
^5B
^�B
^jB
^�B
^�B
_B
_B
_B
_�B
_�B
`B
`vB
`\B
abB
a-B
aHB
a�B
a�B
a�B
b�B
b�B
b�B
b�B
cTB
c�B
c�B
d@B
d@B
dtB
d�B
e,B
e`B
e,B
e`B
e`B
e�B
fB
f�B
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
hXB
h�B
iyB
i_B
i�B
j0B
j0B
jKB
j0B
jB
j�B
kB
kB
kkB
k�B
k�B
l=B
l"B
l�B
m)B
m]B
m�B
m�B
nB
n/B
n/B
ncB
n�B
oB
o5B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
qB
qAB
qvB
q�B
r-B
r-B
r|B
r�B
r�B
r�B
sB
s�B
s�B
s�B
tTB
tnB
t�B
t�B
uB
u?B
uZB
u�B
u�B
u�B
vFB
vFB
vzB
v�B
wLB
w�B
xB
xRB
x�B
y>B
y�B
y�B
y�B
y�B
zDB
zDB
z�B
zxB
z�B
z�B
z�B
z�B
{B
z�B
{0B
{dB
{B
{�B
{�B
|B
|B
|PB
|PB
|�B
|�B
|�B
}B
}VB
}�B
}�B
}�B
~(B
~BB
~wB
~�B
~�B
~�B
~�B
B
�B
�B
}B
�B
�B
� B
�B
�B
��B
��B
��B
��B
��B
��B
� B
�B
�UB
��B
��B
��B
��B
��B
��B
��B
�B
�[B
��B
��B
��B
��B
�GB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�3B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221209184102  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221209184117  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221209184118  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221209184118                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221209184119  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221209184119  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20221209185759                      G�O�G�O�G�O�                