CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:26:29Z creation;2022-06-04T19:26:30Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192629  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ZA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ه�FZC�1   @ه�;Z��@+���-V�d"��`B1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @���A   A   A@  A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�33B���B���B���B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  C   C  C  C  C  C
33C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C;�fC>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�C3DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ D�|�D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܃3D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D��D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D� D��D�  D�@ D� D���D���D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�3D�@ D�|�D��D���D�<�D�3D��fD�3D�C3D�3D��3D�3D�@ D� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @p�@z=q@��@��A�\A>�\A^�\A~�\A�G�A�{A�{A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B�B�B���B�B���B���B���B���B���B���B���B�B�B���B���B���B���B���B���B���B���B���B���B���B�B랹B���B���B���B���B���C��C��C��C��C
)C��C��C��C��C��C�\C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C6�C8�C9��C;�\C=��C?�\CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C`�Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�@RD�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�y�DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D܀RDܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�9�D�}D��D���D�9�D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D���D�9�D�}D�D��D�=D�}D�D��D�=D�}D�D� RD�@RD�}D��D��D�=D�}D���D���D�=D�}D�D��D�=D�y�D�D��D�=D�y�D�D� RD�=D�y�D��D���D�9�D�RD�ÅD� RD�@RD�RD��RD� RD�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�2aA�2aA�4nA�6A�5�A�5tA�6�A�8RA�:*A�:*A�:^A�;dA�?}A�<�A�=�A�1�A�%�A�"hA�
�A��;A��A�|PAՕ�A��A���Aӆ�AӉlAӨ�A��UAӴ�Aӝ~AӍ�Aӂ�A�e`A��A�k�A�\�A�]/A��Aɓ@A�]�A�_AŽ�A�AÜ�A���A���A�"�A�A��oA��<A�sMA�XA�@A�%A��5A��{A���A���A�I�A���A���A��A�CA��^A���A���A���A�eA�\�A���A�چA��MA��)A���A�!�A��\A���A�h>A���A�v�A���A��A�.A�qA���A��A�e�A�t�A���A�S�A~|�A|�)Aw��At��AoϫAj�-Ad��Ab8A`�qA]X�AZqAT��AN�&AMD�AK'RAH�/AF��ACMA>/�A<�A;$A:�7A9�kA8�A6e�A5j�A4<6A3�tA2V�A0�zA.oiA.=�A-xlA-�{A.,�A/JA.�^A. iA-\)A,��A,E9A+�A+�A*��A)��A&�A&A�A%��A%��A%L�A%�A$�	A$V�A${A$	�A$	�A#�EA"v`A ��A!��A"-�A"Q�A!�jA ��A !�A�AGEA?}A�FA,=AT�A^�A� A�A��A��A��A�\A�A@OA�AA�A�A��AB[A�NA�A��A��A��A	lA�)A|�A�+AB[A~A��A�&A��AJA��A|�A*�A�A��Ag�AH�A5?A~�A��A�A�A�A�bA[WA&A�A��AH�AeA
��A
��A
+A	�wA	@�A�,A��A��AS&A7�A�0AMAĜA��A>BA��A��A�9A�:AIRA�8AzxA�\A�A �@��^@�o@�e@�G�@��f@��&@�_�@�ϫ@�a@��b@���@��@���@�D�@�@��@���@�Z�@�^5@��C@���@쀝@�{@���@��@�,�@��@�'R@� �@��@�>�@���@�8�@��p@�~(@�F@��@�q@�5?@��@�F@�"�@�5�@�^@�X@�M@��@䀝@�C-@�iD@�|�@�C@��P@��9@��@ޑ�@ݒ:@�D�@�S�@�|�@�)�@�(�@��T@�7L@���@׈f@�1�@��@�|�@��@�j@ԗ�@ӹ�@��|@�@�B[@�C-@ѹ�@���@�GE@��@�X�@���@�C-@��@�0�@̹$@�e@��@�($@��@���@ɗ�@�'�@�g8@ǵt@�4�@Ƈ�@��>@ť@��@��6@�e,@�@O@_@�6@��@��Z@��@�P�@��H@��I@�q�@��@��V@�x@�Z�@��@�K^@��@@�%F@��?@��1@�V�@��@��#@��@@�b�@�+@���@�z�@�`�@�;�@�	@��r@��N@���@�e,@�=�@�	l@���@��@�q@�]d@�*�@��]@���@�H�@��8@��@�E�@�	@�K�@���@��D@�_@�A�@�ی@���@�i�@�"h@��@���@���@��@���@�zx@�H�@���@��@��w@�c�@�	l@�Xy@���@�q@��@�@��@���@�)_@�xl@���@�G�@���@���@�<�@��}@���@�k�@��@�ں@��F@�1�@��{@�
=@��j@��D@�a|@��&@���@�Mj@��@��U@�c @�ԕ@���@�%F@�(@��@��@�<�@��@��k@��@��X@�H@�e@�ϫ@�x�@�e,@��@�ی@���@�-@��q@���@�qv@��@��A@��@��]@��t@�IR@��B@�]d@��@���@� i@���@��L@�I�@�خ@�x�@�/@���@��)@��_@�K^@��@��"@��f@�~�@��+@��@�1�@�ȴ@�{�@�2�@��*@�;d@���@��B@�y>@��@���@�e�@�N<@�$t@�Ɇ@���@�6@��H@�l�@��@��@�C-@���@�j�@�C�@�"�@��M@���@�2�@�@��@��Z@�ƨ@�Z�@���@��@���@�z�@�~@���@�m]@�33@�(@���@���@���@�h
@�O@��j@�x@� \@��)@��A@�+k@��@��@��j@���@���@�o @�G�@�'�@�@��|@��s@�^5@�V@~��@~��@}rG@|֡@|M@{��@{��@{�@{C�@{C@z�H@zTa@y\�@y;@x�E@x��@xq@x@w4�@v:*@u�@uk�@t��@tq@t�@sݘ@s�K@s��@s9�@r��@rZ�@q�@qN<@p��@pQ�@o�@oA�@o i@o�@o�@n4@mF@m�@l��@k�m@k��@kW?@j�'@jGE@i�@i�@i�X@ik�@iJ�@iq@h�U@h�Y@g��@gJ#@f��@e�@eJ�@d��@d�u@d��@dq@d�@cs@cY@b��@bh
@b5?@a��@a:�@`Ĝ@`[�@`�@_�@_��@_b�@^{�@]�H@]Dg@]�@\�)@\u�@\@[dZ@[�@Z�8@Z�c@Z��@Y��@Yu�@Y!�@X��@X�9@Xe�@W�W@W�:@W33@V��@V;�@U�D@U@U��@U!�@T�5@T�@TFt@TG@S9�@R��@R� @RC�@R@Q�N@Q��@QF@Q+@Q@@P�@P�@PV�@P�@P�@O��@Ot�@N�"@NV@N_@M��@M��@M��@Mu�@Ma�@M0�@L�@L4n@K��@KZ�@K;d@KC@J�!@JL0@J�@I�7@I!�@Hی@H�e@Hw�@HC-@H@G��@G�	@G@O@G!-@G�@F�c@F��@FTa@F	@E��@E[W@E�@D�U@D�.@Dc�@DA�@D7@C�}@C_p@C@B�B@B:*@B4@A�@A�@A��@A��@A�X@Af�@@�@@V�@?�r@?~�@?X�@?RT@?Y@>��@>�A@>R�@=��@=��@=%F@<�|@<�E@<�9@<�I@<r�@</�@;�&@;��@;H�@:��@:\�@:�@9��@9�z@9e,@9�@8�E@8_@7� @7\)@6�h@6��@6Ta@5�j@5��@5�@4�@4��@4_@4@3��@3��@3n/@3�@2�m@2��@2a|@2Ov@1�@1��@1��@1�~@1x�@1F@1@@0�@0�z@0j@0D�@0$@0M@/��@/dZ@/Y@.�B@.l�@.R�@.?@.�@-�Z@-�-@-/@,��@,�.@,I�@,�@+خ@+��@+�:@+e�@+F�@+Y@*�m@*8�@)��@)��@)�=@)��@)u�@)<6@)%F@(�@(��@(��@(h�@(*�@'��@'�@@'��@'H�@&�@&��@&!�@%�@%�N@%�@%#�@%�@$��@$��@$�z@$w�@$7�@$7@#�+@#��@#�:@#iD@#W?@#1�@"��@"�,@"��@"��@"d�@"M�@"O@!�j@!��@!@!�@ ֡@ ��@ ��@ �@ �o@ �@�g@�F@��@J#@�@� @}V@ff@GE@��@<6@�@��@��@��@_@�@�W@��@C@�L@��@��@M�@)�@u@��@/@ی@�@g8@M@D�@A�@1'@�@��@n/@9�@�@��@��@�\@��@v�@n�@@�@�>@�=@��@�@��@�9@q@1'@M@�@��@�{@\)@+@��@��@��@��@��@s�@6�@	@�.@ϫ@��@o @2a@�@�_@l"@:�@4n@6@"h@��@�@��@��@l�@6z@�@�2@��@�1@�r@�A@z@V@B[@u@�@�9@�'@c@0�@V@��@�e@��@r�@j@`�@U2@N�@N�@H@4n@�@��@�@�:@g�@;d@
�M@
�@
�r@
i�@
5?@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�2aA�2aA�4nA�6A�5�A�5tA�6�A�8RA�:*A�:*A�:^A�;dA�?}A�<�A�=�A�1�A�%�A�"hA�
�A��;A��A�|PAՕ�A��A���Aӆ�AӉlAӨ�A��UAӴ�Aӝ~AӍ�Aӂ�A�e`A��A�k�A�\�A�]/A��Aɓ@A�]�A�_AŽ�A�AÜ�A���A���A�"�A�A��oA��<A�sMA�XA�@A�%A��5A��{A���A���A�I�A���A���A��A�CA��^A���A���A���A�eA�\�A���A�چA��MA��)A���A�!�A��\A���A�h>A���A�v�A���A��A�.A�qA���A��A�e�A�t�A���A�S�A~|�A|�)Aw��At��AoϫAj�-Ad��Ab8A`�qA]X�AZqAT��AN�&AMD�AK'RAH�/AF��ACMA>/�A<�A;$A:�7A9�kA8�A6e�A5j�A4<6A3�tA2V�A0�zA.oiA.=�A-xlA-�{A.,�A/JA.�^A. iA-\)A,��A,E9A+�A+�A*��A)��A&�A&A�A%��A%��A%L�A%�A$�	A$V�A${A$	�A$	�A#�EA"v`A ��A!��A"-�A"Q�A!�jA ��A !�A�AGEA?}A�FA,=AT�A^�A� A�A��A��A��A�\A�A@OA�AA�A�A��AB[A�NA�A��A��A��A	lA�)A|�A�+AB[A~A��A�&A��AJA��A|�A*�A�A��Ag�AH�A5?A~�A��A�A�A�A�bA[WA&A�A��AH�AeA
��A
��A
+A	�wA	@�A�,A��A��AS&A7�A�0AMAĜA��A>BA��A��A�9A�:AIRA�8AzxA�\A�A �@��^@�o@�e@�G�@��f@��&@�_�@�ϫ@�a@��b@���@��@���@�D�@�@��@���@�Z�@�^5@��C@���@쀝@�{@���@��@�,�@��@�'R@� �@��@�>�@���@�8�@��p@�~(@�F@��@�q@�5?@��@�F@�"�@�5�@�^@�X@�M@��@䀝@�C-@�iD@�|�@�C@��P@��9@��@ޑ�@ݒ:@�D�@�S�@�|�@�)�@�(�@��T@�7L@���@׈f@�1�@��@�|�@��@�j@ԗ�@ӹ�@��|@�@�B[@�C-@ѹ�@���@�GE@��@�X�@���@�C-@��@�0�@̹$@�e@��@�($@��@���@ɗ�@�'�@�g8@ǵt@�4�@Ƈ�@��>@ť@��@��6@�e,@�@O@_@�6@��@��Z@��@�P�@��H@��I@�q�@��@��V@�x@�Z�@��@�K^@��@@�%F@��?@��1@�V�@��@��#@��@@�b�@�+@���@�z�@�`�@�;�@�	@��r@��N@���@�e,@�=�@�	l@���@��@�q@�]d@�*�@��]@���@�H�@��8@��@�E�@�	@�K�@���@��D@�_@�A�@�ی@���@�i�@�"h@��@���@���@��@���@�zx@�H�@���@��@��w@�c�@�	l@�Xy@���@�q@��@�@��@���@�)_@�xl@���@�G�@���@���@�<�@��}@���@�k�@��@�ں@��F@�1�@��{@�
=@��j@��D@�a|@��&@���@�Mj@��@��U@�c @�ԕ@���@�%F@�(@��@��@�<�@��@��k@��@��X@�H@�e@�ϫ@�x�@�e,@��@�ی@���@�-@��q@���@�qv@��@��A@��@��]@��t@�IR@��B@�]d@��@���@� i@���@��L@�I�@�خ@�x�@�/@���@��)@��_@�K^@��@��"@��f@�~�@��+@��@�1�@�ȴ@�{�@�2�@��*@�;d@���@��B@�y>@��@���@�e�@�N<@�$t@�Ɇ@���@�6@��H@�l�@��@��@�C-@���@�j�@�C�@�"�@��M@���@�2�@�@��@��Z@�ƨ@�Z�@���@��@���@�z�@�~@���@�m]@�33@�(@���@���@���@�h
@�O@��j@�x@� \@��)@��A@�+k@��@��@��j@���@���@�o @�G�@�'�@�@��|@��s@�^5@�V@~��@~��@}rG@|֡@|M@{��@{��@{�@{C�@{C@z�H@zTa@y\�@y;@x�E@x��@xq@x@w4�@v:*@u�@uk�@t��@tq@t�@sݘ@s�K@s��@s9�@r��@rZ�@q�@qN<@p��@pQ�@o�@oA�@o i@o�@o�@n4@mF@m�@l��@k�m@k��@kW?@j�'@jGE@i�@i�@i�X@ik�@iJ�@iq@h�U@h�Y@g��@gJ#@f��@e�@eJ�@d��@d�u@d��@dq@d�@cs@cY@b��@bh
@b5?@a��@a:�@`Ĝ@`[�@`�@_�@_��@_b�@^{�@]�H@]Dg@]�@\�)@\u�@\@[dZ@[�@Z�8@Z�c@Z��@Y��@Yu�@Y!�@X��@X�9@Xe�@W�W@W�:@W33@V��@V;�@U�D@U@U��@U!�@T�5@T�@TFt@TG@S9�@R��@R� @RC�@R@Q�N@Q��@QF@Q+@Q@@P�@P�@PV�@P�@P�@O��@Ot�@N�"@NV@N_@M��@M��@M��@Mu�@Ma�@M0�@L�@L4n@K��@KZ�@K;d@KC@J�!@JL0@J�@I�7@I!�@Hی@H�e@Hw�@HC-@H@G��@G�	@G@O@G!-@G�@F�c@F��@FTa@F	@E��@E[W@E�@D�U@D�.@Dc�@DA�@D7@C�}@C_p@C@B�B@B:*@B4@A�@A�@A��@A��@A�X@Af�@@�@@V�@?�r@?~�@?X�@?RT@?Y@>��@>�A@>R�@=��@=��@=%F@<�|@<�E@<�9@<�I@<r�@</�@;�&@;��@;H�@:��@:\�@:�@9��@9�z@9e,@9�@8�E@8_@7� @7\)@6�h@6��@6Ta@5�j@5��@5�@4�@4��@4_@4@3��@3��@3n/@3�@2�m@2��@2a|@2Ov@1�@1��@1��@1�~@1x�@1F@1@@0�@0�z@0j@0D�@0$@0M@/��@/dZ@/Y@.�B@.l�@.R�@.?@.�@-�Z@-�-@-/@,��@,�.@,I�@,�@+خ@+��@+�:@+e�@+F�@+Y@*�m@*8�@)��@)��@)�=@)��@)u�@)<6@)%F@(�@(��@(��@(h�@(*�@'��@'�@@'��@'H�@&�@&��@&!�@%�@%�N@%�@%#�@%�@$��@$��@$�z@$w�@$7�@$7@#�+@#��@#�:@#iD@#W?@#1�@"��@"�,@"��@"��@"d�@"M�@"O@!�j@!��@!@!�@ ֡@ ��@ ��@ �@ �o@ �@�g@�F@��@J#@�@� @}V@ff@GE@��@<6@�@��@��@��@_@�@�W@��@C@�L@��@��@M�@)�@u@��@/@ی@�@g8@M@D�@A�@1'@�@��@n/@9�@�@��@��@�\@��@v�@n�@@�@�>@�=@��@�@��@�9@q@1'@M@�@��@�{@\)@+@��@��@��@��@��@s�@6�@	@�.@ϫ@��@o @2a@�@�_@l"@:�@4n@6@"h@��@�@��@��@l�@6z@�@�2@��@�1@�r@�A@z@V@B[@u@�@�9@�'@c@0�@V@��@�e@��@r�@j@`�@U2@N�@N�@H@4n@�@��@�@�:@g�@;d@
�M@
�@
�r@
i�@
5?@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
�pB
��B
��B
��B
�VB
�pB
�pB
��B
��B
��B
�pB
�B
�B
� B
�oB
�7B
�
B
�B
��B
�B
��B
�LB
�\B
}�B
h�B
b�B
lqB
�.B
�
B
�CB
�5B
��B
��B
�IB
��B
��B
�gB
i�B
V�B
YB
c�B
p�B
r�B
u�B
��B
�B
͟B
�VB+QB;dBG�B[qBezBo�B{�B��B�;B��B��B�aB�:Bw�BA�B.�B(�B0�B��B�TB��B��B��B��B�,BYB0�B*0B'RB �B�ByB	�B
�`B
�B
�B
��B
g�B
EmB
2B
$�B
]B
�B
�B	��B	��B	��B	��B	��B	��B	��B	y�B	l�B	[�B	EB	)DB	!|B	dB	_B	B	
�B	�B	fB		�B		�B	�B	B	;0B	@�B	:DB	8�B	9	B	:^B	<�B	FtB	`vB	gmB	{�B	�JB	�nB	��B	�RB	��B	ĶB	��B	�B	��B	�B	�B	ևB	�B	�)B	��B	چB	�B	�MB	��B
�B
6B
,B
(B
�B
#�B
1�B
6�B
=VB
:�B
7�B
4�B
5ZB
9�B
DgB
B�B
B�B
>(B
;0B
:*B
7�B
<�B
>wB
A;B
?B
8RB
6�B
88B
7fB
6�B
6�B
5�B
7�B
<B
;�B
9XB
7�B
8�B
6+B
5tB
3MB
6+B
8B
:xB
;0B
;B
;JB
<�B
<jB
;�B
:�B
9�B
9rB
8�B
8�B
6+B
5%B
3�B
1vB
/�B
/5B
.�B
.cB
-�B
-]B
,�B
,=B
,WB
+�B
,"B
,B
+�B
+6B
+B
*�B
*KB
*B
(�B
'RB
&2B
%,B
#nB
"hB
!-B
�B
]B
�B
QB
?B
�B
�B
�B
�B
:B
�B
�B
�B
�B
�B

�B
	7B
�B
�B
B
 B	��B	�2B	�nB	�B	�aB	�[B	��B	��B	�B	�|B	�hB	��B	��B	�TB	��B	�?B	��B	��B	��B	�`B	��B	��B	�xB	��B	�0B	�DB	�$B	�>B	��B
�B
�B
�B
�B
�B
%B
�B
mB
�B
�B
B
�B
�B
�B
{B
-B
B
�B
�B
�B
�B
�B
�B
AB
'B
uB
�B
AB
�B	�B	�<B	��B
 B
'B
B
 4B
�B
�B
�B
�B
�B
�B
 B
�B
�B
MB
B
GB
�B
�B
aB
�B
�B
�B
�B
�B
�B
;B
�B
�B
;B
B
�B
�B
�B
�B
B
�B
[B
[B
AB
'B
�B
�B
�B
�B
�B
UB
oB
 B
 B
oB
;B
;B
;B
 B
UB
UB
;B
UB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
'B
B
uB
�B
�B
SB
�B
B
�B
�B
�B
�B
�B
+B
_B
_B
�B
�B
�B
�B
�B
�B
�B
�B
fB
	�B
	�B
	�B
	�B

�B
)B
xB
B
JB
~B
~B
�B
PB
�B
�B
�B
B
�B
B
bB
bB
�B
 B
4B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
&B
B
�B
�B
�B
�B
�B
�B
aB
�B
�B
�B
�B
�B
MB
gB
9B
�B
�B
�B
�B

B
�B
sB
B
B
B
_B
�B
KB
B
�B
7B
	B
#B
	B
�B
�B
B
�B
�B
B
B
~B
OB
OB
;B
�B
 BB
 �B
 �B
!�B
!�B
"hB
#:B
#�B
#�B
#�B
$@B
$�B
$�B
%FB
%`B
%`B
%�B
%�B
&2B
&�B
&�B
'RB
'�B
(
B
(�B
)B
)DB
)DB
)_B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,B
,�B
-B
-�B
./B
.cB
.�B
.�B
/B
/OB
/�B
/�B
/�B
0!B
0�B
0�B
1'B
1vB
1�B
1�B
1�B
2B
1�B
2B
2|B
2�B
2�B
2aB
2-B
2B
2�B
3hB
3MB
33B
4nB
49B
4�B
4�B
4�B
5%B
5%B
5?B
5?B
5tB
6`B
6FB
6FB
6`B
6FB
6zB
7LB
7�B
8B
8RB
8�B
9$B
9XB
9rB
9>B
9�B
9�B
:^B
:^B
:�B
;JB
;�B
;�B
;�B
<6B
<6B
<�B
<�B
>wB
>]B
>B
>(B
>wB
>�B
@ B
AB
AUB
A�B
A�B
BB
B'B
BB
BAB
B�B
B�B
C-B
B�B
B�B
CGB
C{B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
EB
E9B
E9B
E�B
E�B
E�B
FB
FYB
F�B
FtB
F�B
GzB
G�B
HKB
H1B
HfB
H�B
H�B
IRB
I�B
IlB
IRB
I�B
J#B
JXB
J�B
J�B
J�B
J�B
K)B
K)B
K�B
K�B
L0B
L0B
L~B
L�B
L�B
L�B
M6B
MB
MPB
NB
N<B
NpB
N�B
N�B
O(B
OvB
O�B
O�B
PbB
R B
S[B
SuB
S�B
S�B
S�B
TB
T�B
T�B
U2B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
VSB
VmB
VmB
VmB
V�B
V�B
V�B
W?B
WsB
W�B
W�B
W�B
X+B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YKB
YB
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[=B
[#B
[	B
[=B
[WB
[qB
[qB
[WB
[WB
[�B
\)B
\�B
\�B
]/B
]dB
]IB
]/B
]/B
]/B
]/B
]/B
]~B
]dB
]�B
]�B
^B
^OB
^OB
^jB
^�B
^�B
^�B
_VB
_�B
_�B
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`vB
`�B
a-B
aB
abB
a�B
a�B
b4B
bhB
bNB
b�B
b�B
cB
c B
cnB
c�B
dB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
eB
eB
e,B
e`B
e�B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
g8B
g8B
gRB
gmB
g�B
g�B
h
B
h�B
h�B
h�B
i*B
i_B
iyB
i�B
i�B
i�B
i�B
jB
j�B
k6B
kQB
kQB
kQB
kkB
k�B
k�B
lB
l"B
l=B
l=B
l�B
m]B
m]B
mCB
m�B
m�B
nB
n�B
n�B
n�B
o B
o�B
o�B
o�B
o�B
pB
p;B
poB
p�B
p�B
p�B
qB
qAB
qAB
q[B
q�B
q�B
q�B
rB
rB
rB
rGB
r�B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
tTB
t�B
t�B
tnB
tnB
t�B
t�B
t�B
t�B
t�B
uZB
v`B
vFB
vFB
vFB
v�B
v�B
wB
w2B
wfB
w�B
x8B
xRB
xRB
xlB
xRB
xlB
x�B
yrB
y�B
y�B
zB
zDB
zDB
zDB
z^B
zxB
z�B
z�B
{B
{dB
{�B
{�B
{�B
{�B
{�B
{�B
|B
|jB
|�B
|�B
|�B
|�B
}<B
}VB
}"B
}"B
}VB
}VB
}�B
}�B
}�B
~(B
~BB
~BB
~BB
~]B
~wB
~�B
~�B
B
HB
cB
�B
�B
�B
��B
��B
��B
��B
��B
�B
� B
�oB
�oB
��B
��B
��B
�B
�B
��B
��B
��B
�B
��B
�'B
�B
�[B
�AB
�uB
��B
�-B
��B
��B
��B
�MB
�3B
�3B
�gB
��B
�B
�B
�B
�B
�B
��B
�B
��B
��B
�9B
�SB
��B
�?B
��B
��B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
�pB
��B
��B
��B
�VB
�pB
�pB
��B
��B
��B
�pB
�B
�B
� B
�oB
�7B
�
B
�B
��B
�B
��B
�LB
�\B
}�B
h�B
b�B
lqB
�.B
�
B
�CB
�5B
��B
��B
�IB
��B
��B
�gB
i�B
V�B
YB
c�B
p�B
r�B
u�B
��B
�B
͟B
�VB+QB;dBG�B[qBezBo�B{�B��B�;B��B��B�aB�:Bw�BA�B.�B(�B0�B��B�TB��B��B��B��B�,BYB0�B*0B'RB �B�ByB	�B
�`B
�B
�B
��B
g�B
EmB
2B
$�B
]B
�B
�B	��B	��B	��B	��B	��B	��B	��B	y�B	l�B	[�B	EB	)DB	!|B	dB	_B	B	
�B	�B	fB		�B		�B	�B	B	;0B	@�B	:DB	8�B	9	B	:^B	<�B	FtB	`vB	gmB	{�B	�JB	�nB	��B	�RB	��B	ĶB	��B	�B	��B	�B	�B	ևB	�B	�)B	��B	چB	�B	�MB	��B
�B
6B
,B
(B
�B
#�B
1�B
6�B
=VB
:�B
7�B
4�B
5ZB
9�B
DgB
B�B
B�B
>(B
;0B
:*B
7�B
<�B
>wB
A;B
?B
8RB
6�B
88B
7fB
6�B
6�B
5�B
7�B
<B
;�B
9XB
7�B
8�B
6+B
5tB
3MB
6+B
8B
:xB
;0B
;B
;JB
<�B
<jB
;�B
:�B
9�B
9rB
8�B
8�B
6+B
5%B
3�B
1vB
/�B
/5B
.�B
.cB
-�B
-]B
,�B
,=B
,WB
+�B
,"B
,B
+�B
+6B
+B
*�B
*KB
*B
(�B
'RB
&2B
%,B
#nB
"hB
!-B
�B
]B
�B
QB
?B
�B
�B
�B
�B
:B
�B
�B
�B
�B
�B

�B
	7B
�B
�B
B
 B	��B	�2B	�nB	�B	�aB	�[B	��B	��B	�B	�|B	�hB	��B	��B	�TB	��B	�?B	��B	��B	��B	�`B	��B	��B	�xB	��B	�0B	�DB	�$B	�>B	��B
�B
�B
�B
�B
�B
%B
�B
mB
�B
�B
B
�B
�B
�B
{B
-B
B
�B
�B
�B
�B
�B
�B
AB
'B
uB
�B
AB
�B	�B	�<B	��B
 B
'B
B
 4B
�B
�B
�B
�B
�B
�B
 B
�B
�B
MB
B
GB
�B
�B
aB
�B
�B
�B
�B
�B
�B
;B
�B
�B
;B
B
�B
�B
�B
�B
B
�B
[B
[B
AB
'B
�B
�B
�B
�B
�B
UB
oB
 B
 B
oB
;B
;B
;B
 B
UB
UB
;B
UB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
'B
B
uB
�B
�B
SB
�B
B
�B
�B
�B
�B
�B
+B
_B
_B
�B
�B
�B
�B
�B
�B
�B
�B
fB
	�B
	�B
	�B
	�B

�B
)B
xB
B
JB
~B
~B
�B
PB
�B
�B
�B
B
�B
B
bB
bB
�B
 B
4B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
&B
B
�B
�B
�B
�B
�B
�B
aB
�B
�B
�B
�B
�B
MB
gB
9B
�B
�B
�B
�B

B
�B
sB
B
B
B
_B
�B
KB
B
�B
7B
	B
#B
	B
�B
�B
B
�B
�B
B
B
~B
OB
OB
;B
�B
 BB
 �B
 �B
!�B
!�B
"hB
#:B
#�B
#�B
#�B
$@B
$�B
$�B
%FB
%`B
%`B
%�B
%�B
&2B
&�B
&�B
'RB
'�B
(
B
(�B
)B
)DB
)DB
)_B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,B
,�B
-B
-�B
./B
.cB
.�B
.�B
/B
/OB
/�B
/�B
/�B
0!B
0�B
0�B
1'B
1vB
1�B
1�B
1�B
2B
1�B
2B
2|B
2�B
2�B
2aB
2-B
2B
2�B
3hB
3MB
33B
4nB
49B
4�B
4�B
4�B
5%B
5%B
5?B
5?B
5tB
6`B
6FB
6FB
6`B
6FB
6zB
7LB
7�B
8B
8RB
8�B
9$B
9XB
9rB
9>B
9�B
9�B
:^B
:^B
:�B
;JB
;�B
;�B
;�B
<6B
<6B
<�B
<�B
>wB
>]B
>B
>(B
>wB
>�B
@ B
AB
AUB
A�B
A�B
BB
B'B
BB
BAB
B�B
B�B
C-B
B�B
B�B
CGB
C{B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
EB
E9B
E9B
E�B
E�B
E�B
FB
FYB
F�B
FtB
F�B
GzB
G�B
HKB
H1B
HfB
H�B
H�B
IRB
I�B
IlB
IRB
I�B
J#B
JXB
J�B
J�B
J�B
J�B
K)B
K)B
K�B
K�B
L0B
L0B
L~B
L�B
L�B
L�B
M6B
MB
MPB
NB
N<B
NpB
N�B
N�B
O(B
OvB
O�B
O�B
PbB
R B
S[B
SuB
S�B
S�B
S�B
TB
T�B
T�B
U2B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
VSB
VmB
VmB
VmB
V�B
V�B
V�B
W?B
WsB
W�B
W�B
W�B
X+B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YKB
YB
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[=B
[#B
[	B
[=B
[WB
[qB
[qB
[WB
[WB
[�B
\)B
\�B
\�B
]/B
]dB
]IB
]/B
]/B
]/B
]/B
]/B
]~B
]dB
]�B
]�B
^B
^OB
^OB
^jB
^�B
^�B
^�B
_VB
_�B
_�B
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`vB
`�B
a-B
aB
abB
a�B
a�B
b4B
bhB
bNB
b�B
b�B
cB
c B
cnB
c�B
dB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
eB
eB
e,B
e`B
e�B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
g8B
g8B
gRB
gmB
g�B
g�B
h
B
h�B
h�B
h�B
i*B
i_B
iyB
i�B
i�B
i�B
i�B
jB
j�B
k6B
kQB
kQB
kQB
kkB
k�B
k�B
lB
l"B
l=B
l=B
l�B
m]B
m]B
mCB
m�B
m�B
nB
n�B
n�B
n�B
o B
o�B
o�B
o�B
o�B
pB
p;B
poB
p�B
p�B
p�B
qB
qAB
qAB
q[B
q�B
q�B
q�B
rB
rB
rB
rGB
r�B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
tTB
t�B
t�B
tnB
tnB
t�B
t�B
t�B
t�B
t�B
uZB
v`B
vFB
vFB
vFB
v�B
v�B
wB
w2B
wfB
w�B
x8B
xRB
xRB
xlB
xRB
xlB
x�B
yrB
y�B
y�B
zB
zDB
zDB
zDB
z^B
zxB
z�B
z�B
{B
{dB
{�B
{�B
{�B
{�B
{�B
{�B
|B
|jB
|�B
|�B
|�B
|�B
}<B
}VB
}"B
}"B
}VB
}VB
}�B
}�B
}�B
~(B
~BB
~BB
~BB
~]B
~wB
~�B
~�B
B
HB
cB
�B
�B
�B
��B
��B
��B
��B
��B
�B
� B
�oB
�oB
��B
��B
��B
�B
�B
��B
��B
��B
�B
��B
�'B
�B
�[B
�AB
�uB
��B
�-B
��B
��B
��B
�MB
�3B
�3B
�gB
��B
�B
�B
�B
�B
�B
��B
�B
��B
��B
�9B
�SB
��B
�?B
��B
��B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105247  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192629  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192630  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192630                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042637  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042637  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                