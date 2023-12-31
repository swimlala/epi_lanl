CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-05T00:56:22Z creation;2022-10-05T00:56:23Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221005005622  20221005011101  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�󸴷��1   @��n]@.ݲ-V�cǥ�S��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BrffBu��B�  B�  B�  B���B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C33C  C�fC
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6L�C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @p�@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Br
=Bu=qB��B���B���B���B���B���B���B���B�B�8RB�k�B���B���B���B���B�B�8RB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�C)C��C�\C	��C��C��C��C��C��C��C�\C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C65�C7�\C9�\C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Ch�Cj�Ck��Cm��Co��Cq�\Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&��D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�@RD�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aڐ.Aڅ�A�r�A�m]A�a�A�YKA�W�A�VA�U�A�Q�A�NA�M�A�M�A�N�A�N�A�NA�NA�M�A�NA�MA�K�A�K^A�K�A�HA�7�A�JA���A���A٬�A�sMA��)Aԕ�A�F�A��AӋxA�ɺA�g8AЈ�A��A�?�A��Aɹ$AŦA�V�A�B�A���A��CA���A�pA�ѷA���A�7A�IRA�i�A�.IA��A��[A���A��A�`�A�.�A��A�=�A�HKA��+A��A�L�A���A�CaA�I�A�)*A���A���A�IRA� �A��'A��A���A�x8A� �A�7�A�y�A�o�A��qA�z�A���A��hA�GA�IA��RA��MA�یA�d�AyOAu%ApȴAo�&An�8AkAc�A`�.A^��A]�A[�AAXCAP��AO$�AN��AN��AM6AL�AK!�AG;dAA�?A;�9A9��A9 �A8��A7�A5�fA3A�A0�mA/�A.J�A,&�A+�A*{JA)�oA(�oA'�FA%�mA%�PA'�A'�.A&�)A$�VA#=qA$PHA%7LA%T�A%�A$�<A#�A"�A"��A"�]A"��A"��A"JA!�;A!�A H�A �A �A��AoiA�$AW�A.IA��A3�A��Ak�A�A��A
=A�~A[WA��AGEA.�A��A5�A��A�&A�OAY�AZ�AVmA�An�A�AK�A�)A_A�pAS&A8A��A��A6A
�DA
�[A
��A
�9A
��A
aA
>�A	��A��A�PAI�A\)A�A�?A�RA�AMjA�A{�A(�A��A�WA��AxlA<6A�A�A�XAf�A�AtTAVA �A ȴA �A �Y@��@���@�y�@�&�@��@��f@�o@��[@��@��E@�>B@��@��@���@�k�@�@�@�F@�,=@�>�@��8@��]@��@��m@�@�C�@�H�@���@�?}@�tT@��@��@��@�f�@�c�@�=�@�͟@��@▼@�o@�oi@��@�"h@���@�V@�#:@��@߶F@�v`@� \@޿�@�`�@�(�@�~@�u@ݑh@�ں@��Z@�f�@�@@��,@ڍ�@�!�@�Vm@�8�@��@�m]@��`@�^5@�R�@�J@��@Ԅ�@�i�@��o@�J#@��@��s@�Q@�7@�Z�@��s@�1@��@Τ�@ι$@Α�@ͯ�@��@��B@̞�@�Xy@��@˞�@��?@�N�@��@Ɍ~@�7L@��5@�xl@ǚk@��@Ŝ@�@Ğ�@�_�@�&�@�Z�@��@��p@�~(@�J@���@��@�z@���@�W?@��6@���@�(@���@���@�� @��\@��r@�,=@��@�@@��$@���@�]d@��9@���@�O�@��@���@��@���@�;@���@�@�@���@�p�@��@��I@�H�@���@�@��@��r@�xl@�`�@�4@�7L@���@���@��x@�~(@�e@���@��F@�l�@��@�~(@���@�=�@�Y@���@�C�@�9X@�;�@�;�@�+k@��)@�n/@��@�c�@��g@�e�@�F�@���@���@�+k@���@��}@��@���@�S�@�Y@���@���@�A�@��@��f@�/@���@���@�#:@��@�J@��@���@�*0@��8@�ѷ@�J�@���@��C@���@�*0@��@���@�/�@��W@��^@�9�@���@�c�@�b@���@�x�@�7L@�V@��@���@�$@��0@���@�A @�&�@��P@��@�5?@��@�G@��o@��@��H@��f@�Vm@�E9@�!-@��@��L@�z�@�!�@���@��+@�$@��@��@��?@���@�c @���@��4@�j@��@���@�g8@�4@��@�+k@���@��^@�a�@�+@��@���@� �@�x@���@��+@�e@�u@���@�s�@��"@��v@���@�h
@�$@��0@��4@�Mj@�;d@�F�@�F@�F�@�\)@�O@��5@��@��*@��S@�|@�o @�j@�_p@�8@��`@��<@�kQ@�c @�GE@�.�@�M@��@�x@�2a@�o@��@�V�@��@��&@��S@�c�@�*0@���@��_@�K^@��@���@�y�@�W?@���@�R�@�<�@�3�@�	@���@�}�@�Q�@��@�͟@���@�v�@�S�@���@�@��x@�C-@�"h@��@�1@� @]�@~�@}<6@|$@{��@{dZ@z��@z;�@yj@x��@w�w@wt�@wg�@wO@v�B@vGE@u�9@u��@ua�@uA @u&�@u;@u;@t�	@t��@t�Y@tM@s��@r�1@q��@qhs@qF@p�p@pI�@p<�@p~@o�P@n�@n�@m��@m��@m|@m4@lU2@l �@l@k��@kA�@k�@j��@je@iԕ@i��@i8�@i�@h�	@h��@h�5@hی@h��@htT@hl"@hK^@h,=@g�k@g
=@f��@e�o@e�-@e��@e�@e}�@ew2@eB�@e+�@e	l@d��@dz�@c�W@cZ�@cS@b�r@a��@ap�@aG�@a5�@`�@`I�@`-�@`*�@`"h@_��@^�c@^��@^d�@^$�@^u@]�T@]��@]A @\�@\Z@[��@[S�@[6z@['�@[
=@Z��@ZJ�@Y�@Y�~@Y+@X�`@X��@X��@X]d@XPH@X?�@X*�@X1@W�A@W�
@W�@Wt�@W�@VZ�@T�@Tz�@Ty>@S�A@S�q@Sx@R�@Q��@Q�@Q�T@Q�@Q�>@Q�T@Q�@O�r@O\)@Oo@N��@NJ�@M�S@Mw2@MIR@M�@L"h@K�@K�*@Kqv@J��@I��@IF@H�@HQ�@GdZ@G33@GS@F��@F{@DĜ@C�@CP�@CC@B�y@B�@B��@Bff@A�'@@��@@��@?��@?�@?Z�@>ȴ@>s�@=�d@=L�@<��@<�@<_@;��@;��@;�@;s@;X�@;F�@;1�@;�@:��@:��@:�b@:l�@:E�@:�@9T�@8��@8g8@7�r@7�@7iD@7n/@7l�@7a@7.I@6��@6�h@6�x@6��@6ȴ@6��@6��@6J@5�H@5��@5m]@5^�@5T�@5A @5L�@5\�@5S&@4�5@3��@3�@3�@2��@2�6@2�@2�x@2s�@2M�@1�d@0�@02�@/˒@/.I@.��@.:*@.�@-��@-ԕ@-�h@,Ɇ@,�.@+o�@+!-@*��@*��@*~�@*�@)�^@)}�@)IR@)�@(Ɇ@(bN@(*�@(�@'ƨ@'�{@'g�@'Mj@&�y@&v�@&B[@& �@%�@%�3@%�'@%o @$�@$V�@$�@#�@#~�@#s@#l�@#e�@#J#@#33@#@O@"��@"�B@"��@"z@"8�@!ԕ@!�C@!�7@!;@ ��@ ?�@ �@�6@�w@��@;d@��@��@�@�@ff@�Z@��@��@��@�~@��@c�@+�@��@Ft@~@  @�0@a@F�@8@"�@o@S@�]@��@��@&�@�D@�X@@@�f@��@�@��@�U@�I@�@r�@K^@6@$@M@�@�A@�0@.I@͟@�}@i�@0U@e@�D@�@��@�n@f�@ \@��@w�@A�@ �@$@~@��@�g@��@g�@O@J#@4�@�X@��@�+@;�@�^@�@��@�@e,@F@+@ی@�@e�@@��@�V@�P@��@o�@_p@C�@�@�@� @��@d�@J�@6�@!�@�@�@@��@��@��@��@e,@Dg@ѷ@��@��@��@��@D�@x@�@�&@|�@RT@@O@�@�@Y@
�,@
��@
�F@
C�@
O@	��@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aڐ.Aڅ�A�r�A�m]A�a�A�YKA�W�A�VA�U�A�Q�A�NA�M�A�M�A�N�A�N�A�NA�NA�M�A�NA�MA�K�A�K^A�K�A�HA�7�A�JA���A���A٬�A�sMA��)Aԕ�A�F�A��AӋxA�ɺA�g8AЈ�A��A�?�A��Aɹ$AŦA�V�A�B�A���A��CA���A�pA�ѷA���A�7A�IRA�i�A�.IA��A��[A���A��A�`�A�.�A��A�=�A�HKA��+A��A�L�A���A�CaA�I�A�)*A���A���A�IRA� �A��'A��A���A�x8A� �A�7�A�y�A�o�A��qA�z�A���A��hA�GA�IA��RA��MA�یA�d�AyOAu%ApȴAo�&An�8AkAc�A`�.A^��A]�A[�AAXCAP��AO$�AN��AN��AM6AL�AK!�AG;dAA�?A;�9A9��A9 �A8��A7�A5�fA3A�A0�mA/�A.J�A,&�A+�A*{JA)�oA(�oA'�FA%�mA%�PA'�A'�.A&�)A$�VA#=qA$PHA%7LA%T�A%�A$�<A#�A"�A"��A"�]A"��A"��A"JA!�;A!�A H�A �A �A��AoiA�$AW�A.IA��A3�A��Ak�A�A��A
=A�~A[WA��AGEA.�A��A5�A��A�&A�OAY�AZ�AVmA�An�A�AK�A�)A_A�pAS&A8A��A��A6A
�DA
�[A
��A
�9A
��A
aA
>�A	��A��A�PAI�A\)A�A�?A�RA�AMjA�A{�A(�A��A�WA��AxlA<6A�A�A�XAf�A�AtTAVA �A ȴA �A �Y@��@���@�y�@�&�@��@��f@�o@��[@��@��E@�>B@��@��@���@�k�@�@�@�F@�,=@�>�@��8@��]@��@��m@�@�C�@�H�@���@�?}@�tT@��@��@��@�f�@�c�@�=�@�͟@��@▼@�o@�oi@��@�"h@���@�V@�#:@��@߶F@�v`@� \@޿�@�`�@�(�@�~@�u@ݑh@�ں@��Z@�f�@�@@��,@ڍ�@�!�@�Vm@�8�@��@�m]@��`@�^5@�R�@�J@��@Ԅ�@�i�@��o@�J#@��@��s@�Q@�7@�Z�@��s@�1@��@Τ�@ι$@Α�@ͯ�@��@��B@̞�@�Xy@��@˞�@��?@�N�@��@Ɍ~@�7L@��5@�xl@ǚk@��@Ŝ@�@Ğ�@�_�@�&�@�Z�@��@��p@�~(@�J@���@��@�z@���@�W?@��6@���@�(@���@���@�� @��\@��r@�,=@��@�@@��$@���@�]d@��9@���@�O�@��@���@��@���@�;@���@�@�@���@�p�@��@��I@�H�@���@�@��@��r@�xl@�`�@�4@�7L@���@���@��x@�~(@�e@���@��F@�l�@��@�~(@���@�=�@�Y@���@�C�@�9X@�;�@�;�@�+k@��)@�n/@��@�c�@��g@�e�@�F�@���@���@�+k@���@��}@��@���@�S�@�Y@���@���@�A�@��@��f@�/@���@���@�#:@��@�J@��@���@�*0@��8@�ѷ@�J�@���@��C@���@�*0@��@���@�/�@��W@��^@�9�@���@�c�@�b@���@�x�@�7L@�V@��@���@�$@��0@���@�A @�&�@��P@��@�5?@��@�G@��o@��@��H@��f@�Vm@�E9@�!-@��@��L@�z�@�!�@���@��+@�$@��@��@��?@���@�c @���@��4@�j@��@���@�g8@�4@��@�+k@���@��^@�a�@�+@��@���@� �@�x@���@��+@�e@�u@���@�s�@��"@��v@���@�h
@�$@��0@��4@�Mj@�;d@�F�@�F@�F�@�\)@�O@��5@��@��*@��S@�|@�o @�j@�_p@�8@��`@��<@�kQ@�c @�GE@�.�@�M@��@�x@�2a@�o@��@�V�@��@��&@��S@�c�@�*0@���@��_@�K^@��@���@�y�@�W?@���@�R�@�<�@�3�@�	@���@�}�@�Q�@��@�͟@���@�v�@�S�@���@�@��x@�C-@�"h@��@�1@� @]�@~�@}<6@|$@{��@{dZ@z��@z;�@yj@x��@w�w@wt�@wg�@wO@v�B@vGE@u�9@u��@ua�@uA @u&�@u;@u;@t�	@t��@t�Y@tM@s��@r�1@q��@qhs@qF@p�p@pI�@p<�@p~@o�P@n�@n�@m��@m��@m|@m4@lU2@l �@l@k��@kA�@k�@j��@je@iԕ@i��@i8�@i�@h�	@h��@h�5@hی@h��@htT@hl"@hK^@h,=@g�k@g
=@f��@e�o@e�-@e��@e�@e}�@ew2@eB�@e+�@e	l@d��@dz�@c�W@cZ�@cS@b�r@a��@ap�@aG�@a5�@`�@`I�@`-�@`*�@`"h@_��@^�c@^��@^d�@^$�@^u@]�T@]��@]A @\�@\Z@[��@[S�@[6z@['�@[
=@Z��@ZJ�@Y�@Y�~@Y+@X�`@X��@X��@X]d@XPH@X?�@X*�@X1@W�A@W�
@W�@Wt�@W�@VZ�@T�@Tz�@Ty>@S�A@S�q@Sx@R�@Q��@Q�@Q�T@Q�@Q�>@Q�T@Q�@O�r@O\)@Oo@N��@NJ�@M�S@Mw2@MIR@M�@L"h@K�@K�*@Kqv@J��@I��@IF@H�@HQ�@GdZ@G33@GS@F��@F{@DĜ@C�@CP�@CC@B�y@B�@B��@Bff@A�'@@��@@��@?��@?�@?Z�@>ȴ@>s�@=�d@=L�@<��@<�@<_@;��@;��@;�@;s@;X�@;F�@;1�@;�@:��@:��@:�b@:l�@:E�@:�@9T�@8��@8g8@7�r@7�@7iD@7n/@7l�@7a@7.I@6��@6�h@6�x@6��@6ȴ@6��@6��@6J@5�H@5��@5m]@5^�@5T�@5A @5L�@5\�@5S&@4�5@3��@3�@3�@2��@2�6@2�@2�x@2s�@2M�@1�d@0�@02�@/˒@/.I@.��@.:*@.�@-��@-ԕ@-�h@,Ɇ@,�.@+o�@+!-@*��@*��@*~�@*�@)�^@)}�@)IR@)�@(Ɇ@(bN@(*�@(�@'ƨ@'�{@'g�@'Mj@&�y@&v�@&B[@& �@%�@%�3@%�'@%o @$�@$V�@$�@#�@#~�@#s@#l�@#e�@#J#@#33@#@O@"��@"�B@"��@"z@"8�@!ԕ@!�C@!�7@!;@ ��@ ?�@ �@�6@�w@��@;d@��@��@�@�@ff@�Z@��@��@��@�~@��@c�@+�@��@Ft@~@  @�0@a@F�@8@"�@o@S@�]@��@��@&�@�D@�X@@@�f@��@�@��@�U@�I@�@r�@K^@6@$@M@�@�A@�0@.I@͟@�}@i�@0U@e@�D@�@��@�n@f�@ \@��@w�@A�@ �@$@~@��@�g@��@g�@O@J#@4�@�X@��@�+@;�@�^@�@��@�@e,@F@+@ی@�@e�@@��@�V@�P@��@o�@_p@C�@�@�@� @��@d�@J�@6�@!�@�@�@@��@��@��@��@e,@Dg@ѷ@��@��@��@��@D�@x@�@�&@|�@RT@@O@�@�@Y@
�,@
��@
�F@
C�@
O@	��@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
�2B
��B
��B
��B
��B
��B
��B
�zB
�`B
��B
��B
�zB
�zB
�zB
�zB
�zB
�`B
�`B
�`B
�`B
�`B
�B
��B
�B
�B
�B
��B
�(BSB5�B9>B@ BH�BZ�BuZBy$B.B�3B��B��B/�Bb�B_pBW�BT�BT�BR�B]�BW�B`�B^BZ7BT�BN<BMBJ�BF�BAB3�B2�B;JB5�B%�BmB�B�`B�DB�B�B�B�HB��B��B�]B��B�:B�Bs�BgB]�BLJB-�B=B
��B
өB
��B
�kB
�B
QNB
D�B
�B	��B	� B	��B	�aB	��B	��B	p�B	\]B	RB	J#B	?HB	-�B	�B	�B	mB	�B�.B��B�MB�BخB��B�SBĶB�?B�BˬB͹B�BΥB�uB��B�-B�vB�tB��B	�B	mB	B	>B	S[B	WsB	R�B	VmB	zxB	�hB	��B	�B	�~B	��B	��B	��B	��B	�]B	�B	�aB	��B	�EB	�B	�B	ޞB	��B	�B	ڠB	�QB	��B	�B	�HB	�~B	�pB	�B	��B	�B	��B	��B	�FB	�tB	�
B	�XB	�8B	�mB	�B	�CB	�B	��B	�OB	�AB	�AB	��B	��B	��B	�WB	��B	�$B	��B	��B	��B	��B	�B	�6B	�B	�B	�$B	��B
�B
3B
 B
�B
�B
UB	��B	��B	��B
�B
�B
DB

	B

�B
�B

=B
�B
jB
�B
�B
BB
VB
�B
�B
�B
�B

=B
	�B
	�B
	�B

	B
B
�B
B
B
�B
{B
GB
uB
'B
-B
�B
{B
B
B
B
;B
 iB
  B	�.B	��B	��B	��B	��B	��B	�6B	��B	��B	�dB	�B	�B	�B	�'B	��B	��B	�B	�aB	�B	�B	��B	��B	�hB	��B	��B	�aB	��B	�UB	��B	�B	� B	�OB	�B	� B	� B	�]B	�iB	�B	�B	�OB	�OB	�5B	�!B	��B	��B	�IB	�B	�=B	��B	�B	��B	��B	��B	��B	�}B	�B	�;B	��B	��B	�B	��B	�OB	��B	�B	�B	�wB	��B	�B	��B	�B	�[B	�AB	�vB	�B	��B	�GB	��B	�B	��B	�B	�TB	�nB	�B	�3B	�B	��B	�B	��B	�2B	��B	��B	�`B	�2B	��B	�xB	�B	�rB	��B	��B	��B	��B	�VB	��B	�B	�B	��B	��B	��B	�.B
 OB
UB
UB
�B
[B
�B
�B
[B
B
�B
9B
�B
B
%B
YB
B
tB
�B
�B
%B
�B
%B
?B
%B
%B
tB
EB
zB
�B
KB
�B
	7B

=B

�B
�B
�B
DB

#B
	�B
	lB
	�B
B
B
"B
<B
"B
�B
�B
dB
xB

�B
	�B
	�B
	�B

	B
	�B
	�B
	�B
	�B
	�B

	B

#B
�B
�B
DB

�B

�B

�B

�B

�B
^B
�B
�B
�B
�B
�B
bB
B
�B
�B
hB
B
 B
 B
�B
�B
�B
�B
hB
4B
�B
�B
}B
}B
�B
�B
 B
�B
 B
 B
:B
�B
�B
�B
B
uB
�B
�B
�B
�B
�B
,B
�B
�B
�B
B
B
B
�B
9B
mB
�B
QB
1B
KB
�B
�B
�B
B
�B
B
eB
�B
7B
=B
IB
5B
B
OB
B
B
B
B
;B
�B
�B
�B
�B
�B
!HB
"4B
"NB
#:B
$&B
$B
$B
#�B
#�B
$B
$�B
%,B
%�B
'RB
'B
&LB
&LB
&�B
'B
'�B
(>B
)�B
*B
+�B
+�B
+QB
+�B
,qB
,�B
,�B
,�B
-CB
-]B
-wB
-�B
-�B
.�B
/ B
.�B
.�B
/ B
.�B
/B
/5B
/�B
0B
0oB
0�B
0UB
1B
1B
1'B
0�B
1B
1'B
1B
1'B
1'B
1[B
1�B
1�B
1�B
2�B
33B
3�B
3hB
4B
4B
4B
4B
49B
5B
4�B
5�B
7�B
7�B
8B
8B
8B
88B
8RB
88B
8lB
8�B
8�B
8�B
9$B
9$B
9XB
9rB
9rB
9�B
:B
:B
:^B
:^B
:^B
:�B
9�B
:DB
:�B
;B
;0B
;B
;B
;dB
;�B
<6B
<�B
<�B
=<B
=qB
>B
>�B
?B
>�B
?B
>�B
>�B
>BB
>�B
?B
?.B
?.B
@B
@�B
@�B
@�B
@�B
AB
A�B
A�B
A�B
A�B
B�B
CB
CaB
C�B
C�B
C�B
C�B
C�B
C�B
DB
C�B
C�B
C�B
C�B
D�B
D�B
D�B
ESB
E�B
F%B
F%B
F%B
F?B
FtB
FYB
FtB
FYB
F�B
G_B
HB
G�B
HB
H1B
HKB
HfB
H�B
H�B
IRB
I�B
I�B
I�B
IlB
IlB
I�B
J#B
J#B
J#B
J	B
I�B
J	B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K)B
K�B
MB
L�B
L�B
L�B
MB
M6B
MjB
N"B
NVB
NVB
NVB
NpB
N"B
N�B
OB
N�B
N�B
N�B
N�B
N�B
NpB
NpB
N�B
O\B
OvB
O�B
OvB
O\B
O�B
PbB
PHB
Q B
Q B
Q4B
QhB
QB
PbB
QB
Q�B
RTB
RoB
R�B
RoB
RTB
R�B
T,B
TaB
TFB
T,B
T,B
T�B
U�B
U�B
VB
U�B
V9B
VSB
VSB
V�B
W
B
W?B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
XEB
XyB
YB
Y�B
Z�B
Z�B
[	B
[#B
[WB
\B
]/B
]dB
]~B
^B
^5B
^OB
^�B
`B
`BB
a-B
a|B
abB
a-B
aB
a�B
a�B
a�B
a�B
b�B
a�B
a�B
a�B
bB
a�B
a�B
bB
bhB
bNB
bNB
a�B
a�B
`�B
a-B
aHB
a|B
a�B
b4B
b�B
c B
c B
cTB
c B
b�B
c B
c�B
c�B
dB
d@B
dZB
dtB
d�B
e�B
e�B
fLB
gB
gmB
g�B
gmB
g�B
g�B
g�B
g�B
h
B
h
B
g�B
g�B
i�B
i*B
jB
jB
jB
jeB
jKB
jB
jKB
j0B
j�B
k�B
kkB
k�B
k�B
k�B
lB
lB
l=B
l�B
l�B
m]B
mwB
m�B
m�B
m�B
ncB
ncB
o B
o B
oB
o5B
p!B
p;B
poB
pUB
poB
pUB
poB
p�B
p�B
qAB
qvB
q�B
q�B
rGB
raB
raB
r�B
r�B
r�B
r�B
r�B
sB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
uB
u%B
uZB
uZB
uZB
uZB
uZB
uZB
u�B
vB
v�B
v�B
v�B
w2B
wB
wLB
w�B
wfB
wfB
w�B
xB
xRB
x�B
y	B
y>B
y$B
y>B
yXB
yXB
yrB
y�B
y�B
y�B
y�B
zxB
zxB
z�B
z�B
{0B
{JB
{dB
{B
{dB
{B
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}B
|�B
}B
}�B
}�B
}�B
}�B
}�B
~B
}�B
~(B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�4B
�B
�OB
��B
��B
��B
��B
�B
��B
�oB
��B
��B
��B
�'B
�uB
�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
�2B
��B
��B
��B
��B
��B
��B
�zB
�`B
��B
��B
�zB
�zB
�zB
�zB
�zB
�`B
�`B
�`B
�`B
�`B
�B
��B
�B
�B
�B
��B
�(BSB5�B9>B@ BH�BZ�BuZBy$B.B�3B��B��B/�Bb�B_pBW�BT�BT�BR�B]�BW�B`�B^BZ7BT�BN<BMBJ�BF�BAB3�B2�B;JB5�B%�BmB�B�`B�DB�B�B�B�HB��B��B�]B��B�:B�Bs�BgB]�BLJB-�B=B
��B
өB
��B
�kB
�B
QNB
D�B
�B	��B	� B	��B	�aB	��B	��B	p�B	\]B	RB	J#B	?HB	-�B	�B	�B	mB	�B�.B��B�MB�BخB��B�SBĶB�?B�BˬB͹B�BΥB�uB��B�-B�vB�tB��B	�B	mB	B	>B	S[B	WsB	R�B	VmB	zxB	�hB	��B	�B	�~B	��B	��B	��B	��B	�]B	�B	�aB	��B	�EB	�B	�B	ޞB	��B	�B	ڠB	�QB	��B	�B	�HB	�~B	�pB	�B	��B	�B	��B	��B	�FB	�tB	�
B	�XB	�8B	�mB	�B	�CB	�B	��B	�OB	�AB	�AB	��B	��B	��B	�WB	��B	�$B	��B	��B	��B	��B	�B	�6B	�B	�B	�$B	��B
�B
3B
 B
�B
�B
UB	��B	��B	��B
�B
�B
DB

	B

�B
�B

=B
�B
jB
�B
�B
BB
VB
�B
�B
�B
�B

=B
	�B
	�B
	�B

	B
B
�B
B
B
�B
{B
GB
uB
'B
-B
�B
{B
B
B
B
;B
 iB
  B	�.B	��B	��B	��B	��B	��B	�6B	��B	��B	�dB	�B	�B	�B	�'B	��B	��B	�B	�aB	�B	�B	��B	��B	�hB	��B	��B	�aB	��B	�UB	��B	�B	� B	�OB	�B	� B	� B	�]B	�iB	�B	�B	�OB	�OB	�5B	�!B	��B	��B	�IB	�B	�=B	��B	�B	��B	��B	��B	��B	�}B	�B	�;B	��B	��B	�B	��B	�OB	��B	�B	�B	�wB	��B	�B	��B	�B	�[B	�AB	�vB	�B	��B	�GB	��B	�B	��B	�B	�TB	�nB	�B	�3B	�B	��B	�B	��B	�2B	��B	��B	�`B	�2B	��B	�xB	�B	�rB	��B	��B	��B	��B	�VB	��B	�B	�B	��B	��B	��B	�.B
 OB
UB
UB
�B
[B
�B
�B
[B
B
�B
9B
�B
B
%B
YB
B
tB
�B
�B
%B
�B
%B
?B
%B
%B
tB
EB
zB
�B
KB
�B
	7B

=B

�B
�B
�B
DB

#B
	�B
	lB
	�B
B
B
"B
<B
"B
�B
�B
dB
xB

�B
	�B
	�B
	�B

	B
	�B
	�B
	�B
	�B
	�B

	B

#B
�B
�B
DB

�B

�B

�B

�B

�B
^B
�B
�B
�B
�B
�B
bB
B
�B
�B
hB
B
 B
 B
�B
�B
�B
�B
hB
4B
�B
�B
}B
}B
�B
�B
 B
�B
 B
 B
:B
�B
�B
�B
B
uB
�B
�B
�B
�B
�B
,B
�B
�B
�B
B
B
B
�B
9B
mB
�B
QB
1B
KB
�B
�B
�B
B
�B
B
eB
�B
7B
=B
IB
5B
B
OB
B
B
B
B
;B
�B
�B
�B
�B
�B
!HB
"4B
"NB
#:B
$&B
$B
$B
#�B
#�B
$B
$�B
%,B
%�B
'RB
'B
&LB
&LB
&�B
'B
'�B
(>B
)�B
*B
+�B
+�B
+QB
+�B
,qB
,�B
,�B
,�B
-CB
-]B
-wB
-�B
-�B
.�B
/ B
.�B
.�B
/ B
.�B
/B
/5B
/�B
0B
0oB
0�B
0UB
1B
1B
1'B
0�B
1B
1'B
1B
1'B
1'B
1[B
1�B
1�B
1�B
2�B
33B
3�B
3hB
4B
4B
4B
4B
49B
5B
4�B
5�B
7�B
7�B
8B
8B
8B
88B
8RB
88B
8lB
8�B
8�B
8�B
9$B
9$B
9XB
9rB
9rB
9�B
:B
:B
:^B
:^B
:^B
:�B
9�B
:DB
:�B
;B
;0B
;B
;B
;dB
;�B
<6B
<�B
<�B
=<B
=qB
>B
>�B
?B
>�B
?B
>�B
>�B
>BB
>�B
?B
?.B
?.B
@B
@�B
@�B
@�B
@�B
AB
A�B
A�B
A�B
A�B
B�B
CB
CaB
C�B
C�B
C�B
C�B
C�B
C�B
DB
C�B
C�B
C�B
C�B
D�B
D�B
D�B
ESB
E�B
F%B
F%B
F%B
F?B
FtB
FYB
FtB
FYB
F�B
G_B
HB
G�B
HB
H1B
HKB
HfB
H�B
H�B
IRB
I�B
I�B
I�B
IlB
IlB
I�B
J#B
J#B
J#B
J	B
I�B
J	B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K)B
K�B
MB
L�B
L�B
L�B
MB
M6B
MjB
N"B
NVB
NVB
NVB
NpB
N"B
N�B
OB
N�B
N�B
N�B
N�B
N�B
NpB
NpB
N�B
O\B
OvB
O�B
OvB
O\B
O�B
PbB
PHB
Q B
Q B
Q4B
QhB
QB
PbB
QB
Q�B
RTB
RoB
R�B
RoB
RTB
R�B
T,B
TaB
TFB
T,B
T,B
T�B
U�B
U�B
VB
U�B
V9B
VSB
VSB
V�B
W
B
W?B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
XEB
XyB
YB
Y�B
Z�B
Z�B
[	B
[#B
[WB
\B
]/B
]dB
]~B
^B
^5B
^OB
^�B
`B
`BB
a-B
a|B
abB
a-B
aB
a�B
a�B
a�B
a�B
b�B
a�B
a�B
a�B
bB
a�B
a�B
bB
bhB
bNB
bNB
a�B
a�B
`�B
a-B
aHB
a|B
a�B
b4B
b�B
c B
c B
cTB
c B
b�B
c B
c�B
c�B
dB
d@B
dZB
dtB
d�B
e�B
e�B
fLB
gB
gmB
g�B
gmB
g�B
g�B
g�B
g�B
h
B
h
B
g�B
g�B
i�B
i*B
jB
jB
jB
jeB
jKB
jB
jKB
j0B
j�B
k�B
kkB
k�B
k�B
k�B
lB
lB
l=B
l�B
l�B
m]B
mwB
m�B
m�B
m�B
ncB
ncB
o B
o B
oB
o5B
p!B
p;B
poB
pUB
poB
pUB
poB
p�B
p�B
qAB
qvB
q�B
q�B
rGB
raB
raB
r�B
r�B
r�B
r�B
r�B
sB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
uB
u%B
uZB
uZB
uZB
uZB
uZB
uZB
u�B
vB
v�B
v�B
v�B
w2B
wB
wLB
w�B
wfB
wfB
w�B
xB
xRB
x�B
y	B
y>B
y$B
y>B
yXB
yXB
yrB
y�B
y�B
y�B
y�B
zxB
zxB
z�B
z�B
{0B
{JB
{dB
{B
{dB
{B
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}B
|�B
}B
}�B
}�B
}�B
}�B
}�B
~B
}�B
~(B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�4B
�B
�OB
��B
��B
��B
��B
�B
��B
�oB
��B
��B
��B
�'B
�uB
�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221005005609  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221005005622  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221005005623  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221005005623                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221005095628  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221005095628  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221005011101                      G�O�G�O�G�O�                