CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-20T18:43:21Z creation;2022-06-20T18:43:22Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220620184321  20220620185821  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               zA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @����Y 1   @��J�͏@1;�l�C��c�-1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBq33Bx  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�33B���B���B�  B�  B�  B�  BÙ�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC/�fC2  C4  C6  C8  C:  C<  C>�C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bh
=Bp�
Bw��B��B���B���B���B���B���B���B���B�k�B���B�B���B���B���B���B���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-�\C/�\C1��C3��C5��C7��C9��C;��C>�C@�CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Ch�Cj�Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D��D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQ��DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DX �DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Dv �Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��0A���A��vA���A��?A��A�چA�یA�ںA��A��A��A���A���A�˒AХ�AЭ�A�{A�[�A�:^A�,�A�*�A��A���Aψ�A�.A���A��fA��HAΉ�A͍�A˸�A�@�A���AʍPA�33A�y	A�)�Aſ�A�g�A�/OA��AíwA� �A�@A�J�A� A��pA�oiA�ߤA���A��A�NA�چA���A�T�A��<A�=qA�,=A��A�E�A��A�"4A�_�A��TA��{A���A�(�A���A�r�A��A�ffA��]A�1�A��A��[A��A��A���A��XA���A�&�A�:A�u�A���A�̘A�poA�A�A�͟A�H�A�GA��sA���A�}"A���A��_A�K�A{b�AvH�Ap+Aj�&Ad�AatTA`�A_�gA]o AY�4AW��AVl"AQ�sANZ�AMOALAG�AB�eAA�A=��A;�'A94�A7Z�A4��A4SA3<6A2u�A1��A1_A1MA0oA-� A-�qA-��A-zA,�A+  A(�DA'��A'�DA(4�A&�A&=�A&OA&��A&�CA&v�A%��A%D�A$��A$�A#��A$%A#��A"l"A!�hA!:*A �A ��A r�A�AY�A�A��A+�A�A�4A��Ae�A1AxAS�A$�A�A�
A��A{�A<6A�DAƨAU2AS�A�;A�CA�jA2aA�CA�9Aa�AuAOA��A��A��A`�A�AqvA�A �A�xA1�A(�A�]A9XA��A��A��A��A6�A�
A
u�A
	A	��A	JA��A��A�A��A�A�A�A�oA�A��A�ZAcA�A��A�4Au�A�AtTA5�A�$Aa�A ��@��@��E@���@���@�Q�@�c@��@�p;@�xl@�u�@���@���@�1'@���@�A @���@��@�@�Mj@�8@��@��H@��@�!@�6@��@@�qv@��	@�\@��Q@�F@���@�1�@��@���@�ں@��@��@�B�@曦@橓@�zx@��6@�u@�-@�-w@�PH@�#:@���@�[@��@��@�8@�*0@��5@⭬@��@�X�@���@�t�@޺�@�?�@��@�K�@�j@��#@ۤ@@�Vm@��[@ڜx@�tT@�_�@�3�@��Q@ي	@���@�$@׊�@��@֧�@�x@�4@��/@�S�@Ӵ�@�=�@��K@ҩ�@�.�@��@ч�@�O�@�V@Гu@��+@ς�@�,�@��@��@·+@�Ft@��@ͫ�@��H@�%�@˦�@˙�@�O@�%F@��c@���@ʻ�@�/�@�w2@�A�@��@���@Ȣ4@�PH@���@ǥ@�j@�@O@�ߤ@��@�^�@�E9@�Mj@���@ć+@�Z�@ü�@��"@½<@�H�@�S@��@�@�@��Q@��@�Z�@��@���@�p;@� �@��C@�f�@��"@���@�U�@�&�@���@��@�T�@��@�|�@���@�zx@�O�@��@�n�@�)�@���@���@���@�5?@�C@�^5@��H@�zx@� i@��@��@��7@�+@��U@���@�:*@��0@��@��@�@�@�c�@�Q@�=q@�!@��t@�\)@��y@���@��@�z�@�Ft@��@��@�j�@�;d@�C@��`@���@��j@���@���@�/�@���@���@���@���@��x@�}V@�_@�ԕ@�{J@���@��@�:*@��@��#@��9@��{@�rG@�iD@�p�@�X�@�͟@�1�@���@�|@�C�@���@���@��@�r�@�5?@��Z@��@���@�e�@�(�@���@���@�oi@�	�@��@��@�@O@��@��@�?�@��@���@�e�@�@O@�"�@��P@���@��z@��r@�a|@�)�@��@��H@�O@��@���@�!�@���@�1�@��'@�[�@�ݘ@���@�p�@�!�@�͟@�a|@���@���@�G�@��P@�ѷ@���@�8�@���@�e�@��y@���@���@���@�Ta@��@�ݘ@��=@�n/@�l�@�_p@�7L@�"�@�@@��p@�l"@�5?@�e@��@�~@���@���@�@���@�^�@�P�@�'�@��X@��1@���@�]d@�1'@�u@���@���@�9�@��B@�oi@��9@�x@�S�@�"�@��@��m@���@���@�1'@���@�@���@�=@��@�u%@�_�@�/�@��@���@���@�`B@�4�@���@�{�@�e�@�Xy@�W�@�W�@� �@��j@�n/@�@@��"@���@�֡@��L@�h
@�Z�@�Q�@�GE@�=q@�#:@��@��@���@�5�@�
=@�͟@���@�l"@�;�@�"h@��j@�[W@��@��9@��@�oi@�5?@�@�ƨ@���@�a�@�;d@� \@��@��|@��$@�u�@�P@~��@~kQ@}��@}#�@|��@|<�@{�@{a@{e�@{=@{o@z҉@z;�@yY�@x:�@w��@wH�@v�@v�}@vn�@vh
@uL�@t�`@t�4@tq@t[�@tQ�@t@s�*@s�k@s|�@sa@s@O@r��@ru%@r($@q�@q��@q/@q�@p��@p�4@pg8@p6@p*�@p�@o��@o��@o~�@o i@n�@ns�@m�N@mu�@m*0@l��@l>B@k�[@k�V@k�f@kW?@k�@j��@jL0@j=q@j5?@i�@i��@i?}@h��@hD�@g�V@gF�@g�@f�"@f�y@f�@fB[@e�@e�n@ec@ef�@e0�@d��@d!@c��@b��@b}V@b �@a�~@aN<@a+@`7�@_{J@_$t@^�2@^^5@]�z@]�=@]F@\�4@\oi@\2�@[�@@[o�@Z�2@Z^5@Y��@X��@X��@XN�@X'R@Wx@W,�@V��@VJ@U��@U�@Uhs@T�5@TU2@S�:@S(@R�@R
�@Q�n@Q�h@Q4@P��@P�@O��@OY@O�@N�c@N҉@N�@N��@N�@N~�@Np;@NJ�@M�z@M�h@M0�@L�@L��@LQ�@L*�@L�@K��@K��@KC@J$�@I�t@IN<@H�@H%�@Gݘ@G�	@GC@F��@F��@F�x@F� @F��@Fff@Fe@Fu@E�N@E�~@EX@E7L@D�@D�_@DPH@D!@C�&@C�@@B��@B��@Bi�@B�@A��@A��@@Ɇ@?�*@>ߤ@>�h@>��@>Z�@>Q@>?@>#:@>�@>�@>�@=��@=��@=+�@<�@<�[@<tT@<D�@</�@;�6@;iD@;.I@;�@:�H@:�m@:�6@:V@9��@9��@8ѷ@8y>@8%�@7��@7�;@7��@6��@6�+@6u%@5��@5}�@5Y�@5(�@5&�@5q@4�@4�j@4��@4e�@3n/@2V@2O@1�N@1�n@1�@1�@0bN@0Ft@0(�@/��@/@O@.��@.�@.��@.e@-��@-(�@-(�@--w@-#�@,�|@,`�@+�6@+n/@+J#@*�1@*Z�@*-@*�@)�@)x�@):�@(�5@(m�@'�@'�6@'Mj@'�@&xl@%�@%x�@%J�@%A @%!�@$�@$�@#K�@"O@!��@!7L@ �f@ ��@ ��@ �e@ ��@ �u@ �D@ ��@ �@ u�@ q@ q@ j@ j@ h�@ g8@ g8@ bN@ _@ _@ K^@ �@��@� @��@�[@��@n/@6z@@.I@�@ i@�8@��@�!@v�@�@�>@�@u�@IR@*0@;@��@�.@*�@�@�
@�K@�0@��@�q@�@@�@��@�$@��@S�@V@�@�@�~@zx@a�@A @	l@Z@	�@�[@dZ@U�@J#@@O@/�@&@Y@(@��@�@��@�\@~�@a|@?@�z@�@*0@�@��@��@l"@PH@D�@"h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��0A���A��vA���A��?A��A�چA�یA�ںA��A��A��A���A���A�˒AХ�AЭ�A�{A�[�A�:^A�,�A�*�A��A���Aψ�A�.A���A��fA��HAΉ�A͍�A˸�A�@�A���AʍPA�33A�y	A�)�Aſ�A�g�A�/OA��AíwA� �A�@A�J�A� A��pA�oiA�ߤA���A��A�NA�چA���A�T�A��<A�=qA�,=A��A�E�A��A�"4A�_�A��TA��{A���A�(�A���A�r�A��A�ffA��]A�1�A��A��[A��A��A���A��XA���A�&�A�:A�u�A���A�̘A�poA�A�A�͟A�H�A�GA��sA���A�}"A���A��_A�K�A{b�AvH�Ap+Aj�&Ad�AatTA`�A_�gA]o AY�4AW��AVl"AQ�sANZ�AMOALAG�AB�eAA�A=��A;�'A94�A7Z�A4��A4SA3<6A2u�A1��A1_A1MA0oA-� A-�qA-��A-zA,�A+  A(�DA'��A'�DA(4�A&�A&=�A&OA&��A&�CA&v�A%��A%D�A$��A$�A#��A$%A#��A"l"A!�hA!:*A �A ��A r�A�AY�A�A��A+�A�A�4A��Ae�A1AxAS�A$�A�A�
A��A{�A<6A�DAƨAU2AS�A�;A�CA�jA2aA�CA�9Aa�AuAOA��A��A��A`�A�AqvA�A �A�xA1�A(�A�]A9XA��A��A��A��A6�A�
A
u�A
	A	��A	JA��A��A�A��A�A�A�A�oA�A��A�ZAcA�A��A�4Au�A�AtTA5�A�$Aa�A ��@��@��E@���@���@�Q�@�c@��@�p;@�xl@�u�@���@���@�1'@���@�A @���@��@�@�Mj@�8@��@��H@��@�!@�6@��@@�qv@��	@�\@��Q@�F@���@�1�@��@���@�ں@��@��@�B�@曦@橓@�zx@��6@�u@�-@�-w@�PH@�#:@���@�[@��@��@�8@�*0@��5@⭬@��@�X�@���@�t�@޺�@�?�@��@�K�@�j@��#@ۤ@@�Vm@��[@ڜx@�tT@�_�@�3�@��Q@ي	@���@�$@׊�@��@֧�@�x@�4@��/@�S�@Ӵ�@�=�@��K@ҩ�@�.�@��@ч�@�O�@�V@Гu@��+@ς�@�,�@��@��@·+@�Ft@��@ͫ�@��H@�%�@˦�@˙�@�O@�%F@��c@���@ʻ�@�/�@�w2@�A�@��@���@Ȣ4@�PH@���@ǥ@�j@�@O@�ߤ@��@�^�@�E9@�Mj@���@ć+@�Z�@ü�@��"@½<@�H�@�S@��@�@�@��Q@��@�Z�@��@���@�p;@� �@��C@�f�@��"@���@�U�@�&�@���@��@�T�@��@�|�@���@�zx@�O�@��@�n�@�)�@���@���@���@�5?@�C@�^5@��H@�zx@� i@��@��@��7@�+@��U@���@�:*@��0@��@��@�@�@�c�@�Q@�=q@�!@��t@�\)@��y@���@��@�z�@�Ft@��@��@�j�@�;d@�C@��`@���@��j@���@���@�/�@���@���@���@���@��x@�}V@�_@�ԕ@�{J@���@��@�:*@��@��#@��9@��{@�rG@�iD@�p�@�X�@�͟@�1�@���@�|@�C�@���@���@��@�r�@�5?@��Z@��@���@�e�@�(�@���@���@�oi@�	�@��@��@�@O@��@��@�?�@��@���@�e�@�@O@�"�@��P@���@��z@��r@�a|@�)�@��@��H@�O@��@���@�!�@���@�1�@��'@�[�@�ݘ@���@�p�@�!�@�͟@�a|@���@���@�G�@��P@�ѷ@���@�8�@���@�e�@��y@���@���@���@�Ta@��@�ݘ@��=@�n/@�l�@�_p@�7L@�"�@�@@��p@�l"@�5?@�e@��@�~@���@���@�@���@�^�@�P�@�'�@��X@��1@���@�]d@�1'@�u@���@���@�9�@��B@�oi@��9@�x@�S�@�"�@��@��m@���@���@�1'@���@�@���@�=@��@�u%@�_�@�/�@��@���@���@�`B@�4�@���@�{�@�e�@�Xy@�W�@�W�@� �@��j@�n/@�@@��"@���@�֡@��L@�h
@�Z�@�Q�@�GE@�=q@�#:@��@��@���@�5�@�
=@�͟@���@�l"@�;�@�"h@��j@�[W@��@��9@��@�oi@�5?@�@�ƨ@���@�a�@�;d@� \@��@��|@��$@�u�@�P@~��@~kQ@}��@}#�@|��@|<�@{�@{a@{e�@{=@{o@z҉@z;�@yY�@x:�@w��@wH�@v�@v�}@vn�@vh
@uL�@t�`@t�4@tq@t[�@tQ�@t@s�*@s�k@s|�@sa@s@O@r��@ru%@r($@q�@q��@q/@q�@p��@p�4@pg8@p6@p*�@p�@o��@o��@o~�@o i@n�@ns�@m�N@mu�@m*0@l��@l>B@k�[@k�V@k�f@kW?@k�@j��@jL0@j=q@j5?@i�@i��@i?}@h��@hD�@g�V@gF�@g�@f�"@f�y@f�@fB[@e�@e�n@ec@ef�@e0�@d��@d!@c��@b��@b}V@b �@a�~@aN<@a+@`7�@_{J@_$t@^�2@^^5@]�z@]�=@]F@\�4@\oi@\2�@[�@@[o�@Z�2@Z^5@Y��@X��@X��@XN�@X'R@Wx@W,�@V��@VJ@U��@U�@Uhs@T�5@TU2@S�:@S(@R�@R
�@Q�n@Q�h@Q4@P��@P�@O��@OY@O�@N�c@N҉@N�@N��@N�@N~�@Np;@NJ�@M�z@M�h@M0�@L�@L��@LQ�@L*�@L�@K��@K��@KC@J$�@I�t@IN<@H�@H%�@Gݘ@G�	@GC@F��@F��@F�x@F� @F��@Fff@Fe@Fu@E�N@E�~@EX@E7L@D�@D�_@DPH@D!@C�&@C�@@B��@B��@Bi�@B�@A��@A��@@Ɇ@?�*@>ߤ@>�h@>��@>Z�@>Q@>?@>#:@>�@>�@>�@=��@=��@=+�@<�@<�[@<tT@<D�@</�@;�6@;iD@;.I@;�@:�H@:�m@:�6@:V@9��@9��@8ѷ@8y>@8%�@7��@7�;@7��@6��@6�+@6u%@5��@5}�@5Y�@5(�@5&�@5q@4�@4�j@4��@4e�@3n/@2V@2O@1�N@1�n@1�@1�@0bN@0Ft@0(�@/��@/@O@.��@.�@.��@.e@-��@-(�@-(�@--w@-#�@,�|@,`�@+�6@+n/@+J#@*�1@*Z�@*-@*�@)�@)x�@):�@(�5@(m�@'�@'�6@'Mj@'�@&xl@%�@%x�@%J�@%A @%!�@$�@$�@#K�@"O@!��@!7L@ �f@ ��@ ��@ �e@ ��@ �u@ �D@ ��@ �@ u�@ q@ q@ j@ j@ h�@ g8@ g8@ bN@ _@ _@ K^@ �@��@� @��@�[@��@n/@6z@@.I@�@ i@�8@��@�!@v�@�@�>@�@u�@IR@*0@;@��@�.@*�@�@�
@�K@�0@��@�q@�@@�@��@�$@��@S�@V@�@�@�~@zx@a�@A @	l@Z@	�@�[@dZ@U�@J#@@O@/�@&@Y@(@��@�@��@�\@~�@a|@?@�z@�@*0@�@��@��@l"@PH@D�@"h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	��B	��B	��B	�|B	�|B	��B	�aB	�aB	��B	��B	��B	��B	�[B	�5B	��B	�WB	�eB	�>B	��B	�LB	�@B	�OB	�IB	��B	�fB	��B	�XB	��B	�`B	ɠB	�{B	��B	�B
+B
+B
(�B
}B
�sB
�=B
�xB
�:B
��B
��B<�B�B�
B��B# B7B�B;�BF�BqvBn�Bd�BV�BH1B:xB5%B>B4�B(�B"�B"�B
B
=B�>B�0B�B�FB�-Bp!Bg�Bn�B�GButB_�BM6B5tB
��B
�@B
�
B
�JB
�-B
�cB
�7B
�B
z�B
t9B
^OB
K�B
<B
/�B
)B
3B	�dB	��B	�!B	�B	gB	X_B	Q�B	M�B	EB	5%B	+QB	#TB	EB		�B	aB�.B�$B�B�*B��B��B�8B��B�B	 �B	-B	�B	�B	�B	�B	0�B	=�B	?�B	@�B	A�B	?�B	BAB	`vB	ffB	r�B	|�B	uB	|B	��B	��B	��B	��B	�-B	��B	�B	�,B	�	B	�B	��B	�jB	ܒB	�kB	ݲB	��B	�4B	�B	�hB	ڠB	��B	��B	޸B	��B	�vB	�B	�B	�FB	�B	�TB	��B	�ZB	�B	��B	��B	�XB	�B	�)B	�B	�B	�vB	�B	��B	��B	�8B	�$B	��B	�B	��B	��B	��B	�B	�wB	�(B	��B	��B	�}B	�B	��B	��B	��B	��B
B	�B	��B	��B	��B	��B	��B	��B
 iB
  B	��B
 OB
 �B
 B	�.B	��B	�BB	�BB
 iB	��B
 �B
  B	��B	�cB	�B	��B	�<B	�6B	�jB	��B	�ZB	�B	�B	�B	�kB	�B	��B	�B	�"B	��B	�B	��B	�B	�kB	�WB	�=B	��B	�QB	�QB	�"B	��B	�OB	�B	�IB	��B	�B	�B	�B	��B	��B	�B	�5B	�}B	�B	�B	�OB	�OB	�5B	�iB	�B	�B	�"B	�B	��B	��B
AB
UB
 �B	�B	�BB	��B
 iB	��B	�B	�jB	�jB	�qB	��B	�qB	��B	�B	��B	�rB	��B	�	B	�RB	��B	��B	��B	��B	�zB	�`B	�+B	�B	��B	��B	��B	�tB	��B	��B	�tB	�%B	�ZB	��B	�ZB	�tB	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�FB	��B	��B	��B	�B	��B	��B	��B	�LB	��B	�B	�2B	�B	�8B	�lB	�RB	�B	�	B	�XB	�XB	�XB	�	B	��B	�B	��B	�B	��B	�`B	��B	��B	��B	��B	�rB	�rB	��B	�	B	�B	��B	��B	�ZB	�tB	��B	��B	�tB	��B	�FB	�zB	��B	�zB	�FB	�`B	��B	��B	�"B	�wB	��B	��B	��B	��B	�VB	�6B	�JB	��B	��B	��B	�.B	�cB
uB
�B
�B
�B
�B
{B
�B
{B
aB
�B
?B
�B
YB
�B
�B
_B
	�B
�B
	�B

�B
B
�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
\B
�B
�B
}B
 B
 B
�B
.B
�B
�B
�B
B
�B
�B
oB
TB
B
B
 B
�B
�B
�B
�B
.B
}B
�B
�B
�B
:B
�B
�B
�B
,B
�B
�B
�B
�B
�B
�B
�B
�B
9B
SB
mB
�B

B
$B
�B

B
YB
sB
�B
�B
EB
yB
�B
B
eB
�B
�B
�B
7B
QB
�B
�B
�B
�B
�B
�B
#B
WB
�B
�B
�B
dB
�B
�B
�B
�B
�B
�B
B
�B
jB
pB
�B
 B
 'B
 �B
!bB
!�B
"�B
"�B
"�B
"hB
"�B
"�B
#B
#:B
#nB
#�B
#�B
$ZB
$tB
$ZB
$�B
%B
$�B
$�B
$�B
%,B
%`B
%,B
%�B
%�B
&�B
&2B
&LB
&�B
&�B
&�B
'B
'�B
'�B
(>B
'�B
'B
&B
'B
'B
&�B
'B
'8B
'B
'mB
'mB
'�B
(>B
(>B
(�B
(�B
(�B
)�B
)yB
)_B
)�B
)�B
)�B
*eB
*KB
*KB
*B
)�B
*B
*�B
*�B
*�B
*�B
*�B
*�B
+QB
+kB
+�B
+�B
,WB
+�B
+�B
+�B
,B
,"B
,WB
,qB
-B
-CB
-�B
-�B
./B
.B
-�B
.IB
/ B
/�B
0;B
0�B
1'B
0�B
0�B
0�B
0�B
1B
1[B
1vB
1vB
1�B
1vB
1vB
1[B
1'B
1AB
1B
0�B
0B
/iB
/ B
.�B
-�B
-]B
-�B
.cB
.IB
.cB
.�B
.IB
/5B
/�B
/�B
0;B
0oB
0�B
0oB
1�B
2-B
2|B
2�B
3B
2�B
3�B
4�B
4�B
5B
5?B
5�B
5�B
6�B
6�B
7B
7�B
9>B
9rB
9�B
9�B
9�B
:*B
:B
:*B
:B
:^B
:xB
;0B
;JB
;0B
:�B
:*B
:B
9�B
:DB
;B
;B
;JB
;dB
;�B
<PB
<jB
<6B
<B
<jB
<6B
<�B
<�B
=B
=�B
=�B
>B
>B
>(B
>]B
>�B
?B
?�B
?�B
?�B
?�B
@OB
@�B
@�B
A B
A�B
BB
B�B
BuB
BuB
CaB
C�B
C�B
C�B
DB
D�B
D�B
D�B
D�B
D�B
E9B
E�B
E�B
E�B
ESB
E9B
EB
E9B
E9B
E�B
F?B
F?B
FtB
FYB
FYB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
IB
IRB
I�B
JXB
JrB
JXB
JXB
J=B
J=B
JXB
J=B
J=B
JXB
JrB
J�B
KB
KxB
MB
M6B
MB
MB
MB
M�B
M�B
N"B
N"B
M�B
M�B
MB
L�B
MB
MPB
M�B
M�B
M�B
M�B
NB
NB
N�B
N�B
OB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
P.B
PHB
P�B
P�B
QNB
Q�B
Q�B
Q�B
R�B
S�B
T,B
TFB
TaB
T{B
T{B
T�B
T�B
T�B
T�B
T�B
T{B
T�B
UB
UMB
U2B
U�B
U�B
U�B
U�B
VSB
V�B
WsB
X�B
X�B
X�B
Y�B
Y�B
ZB
ZQB
ZQB
Z�B
Z�B
Z�B
[#B
[�B
\B
[�B
[�B
[�B
\B
\)B
[�B
[�B
\B
[�B
\B
\B
]B
]�B
]�B
^B
^B
]�B
^jB
^�B
^�B
^�B
^�B
_pB
_�B
_�B
_�B
`vB
`�B
abB
aHB
a-B
aHB
abB
bNB
b�B
cB
cB
dB
d&B
d@B
d&B
dZB
d�B
d�B
ezB
e�B
fLB
fLB
f�B
f�B
gRB
g�B
hsB
h�B
hXB
hsB
h�B
iDB
jB
kQB
k�B
lB
l=B
l=B
lWB
lqB
lqB
l�B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m)B
mCB
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nIB
n�B
n�B
oB
o�B
o�B
o�B
pB
p!B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
poB
qAB
qB
qB
p�B
p�B
p�B
p�B
p�B
q�B
q�B
rB
raB
raB
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sB
sB
shB
s�B
tB
tnB
t�B
t�B
t�B
u%B
u?B
u?B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	��B	��B	��B	�|B	�|B	��B	�aB	�aB	��B	��B	��B	��B	�[B	�5B	��B	�WB	�eB	�>B	��B	�LB	�@B	�OB	�IB	��B	�fB	��B	�XB	��B	�`B	ɠB	�{B	��B	�B
+B
+B
(�B
}B
�sB
�=B
�xB
�:B
��B
��B<�B�B�
B��B# B7B�B;�BF�BqvBn�Bd�BV�BH1B:xB5%B>B4�B(�B"�B"�B
B
=B�>B�0B�B�FB�-Bp!Bg�Bn�B�GButB_�BM6B5tB
��B
�@B
�
B
�JB
�-B
�cB
�7B
�B
z�B
t9B
^OB
K�B
<B
/�B
)B
3B	�dB	��B	�!B	�B	gB	X_B	Q�B	M�B	EB	5%B	+QB	#TB	EB		�B	aB�.B�$B�B�*B��B��B�8B��B�B	 �B	-B	�B	�B	�B	�B	0�B	=�B	?�B	@�B	A�B	?�B	BAB	`vB	ffB	r�B	|�B	uB	|B	��B	��B	��B	��B	�-B	��B	�B	�,B	�	B	�B	��B	�jB	ܒB	�kB	ݲB	��B	�4B	�B	�hB	ڠB	��B	��B	޸B	��B	�vB	�B	�B	�FB	�B	�TB	��B	�ZB	�B	��B	��B	�XB	�B	�)B	�B	�B	�vB	�B	��B	��B	�8B	�$B	��B	�B	��B	��B	��B	�B	�wB	�(B	��B	��B	�}B	�B	��B	��B	��B	��B
B	�B	��B	��B	��B	��B	��B	��B
 iB
  B	��B
 OB
 �B
 B	�.B	��B	�BB	�BB
 iB	��B
 �B
  B	��B	�cB	�B	��B	�<B	�6B	�jB	��B	�ZB	�B	�B	�B	�kB	�B	��B	�B	�"B	��B	�B	��B	�B	�kB	�WB	�=B	��B	�QB	�QB	�"B	��B	�OB	�B	�IB	��B	�B	�B	�B	��B	��B	�B	�5B	�}B	�B	�B	�OB	�OB	�5B	�iB	�B	�B	�"B	�B	��B	��B
AB
UB
 �B	�B	�BB	��B
 iB	��B	�B	�jB	�jB	�qB	��B	�qB	��B	�B	��B	�rB	��B	�	B	�RB	��B	��B	��B	��B	�zB	�`B	�+B	�B	��B	��B	��B	�tB	��B	��B	�tB	�%B	�ZB	��B	�ZB	�tB	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�FB	��B	��B	��B	�B	��B	��B	��B	�LB	��B	�B	�2B	�B	�8B	�lB	�RB	�B	�	B	�XB	�XB	�XB	�	B	��B	�B	��B	�B	��B	�`B	��B	��B	��B	��B	�rB	�rB	��B	�	B	�B	��B	��B	�ZB	�tB	��B	��B	�tB	��B	�FB	�zB	��B	�zB	�FB	�`B	��B	��B	�"B	�wB	��B	��B	��B	��B	�VB	�6B	�JB	��B	��B	��B	�.B	�cB
uB
�B
�B
�B
�B
{B
�B
{B
aB
�B
?B
�B
YB
�B
�B
_B
	�B
�B
	�B

�B
B
�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
\B
�B
�B
}B
 B
 B
�B
.B
�B
�B
�B
B
�B
�B
oB
TB
B
B
 B
�B
�B
�B
�B
.B
}B
�B
�B
�B
:B
�B
�B
�B
,B
�B
�B
�B
�B
�B
�B
�B
�B
9B
SB
mB
�B

B
$B
�B

B
YB
sB
�B
�B
EB
yB
�B
B
eB
�B
�B
�B
7B
QB
�B
�B
�B
�B
�B
�B
#B
WB
�B
�B
�B
dB
�B
�B
�B
�B
�B
�B
B
�B
jB
pB
�B
 B
 'B
 �B
!bB
!�B
"�B
"�B
"�B
"hB
"�B
"�B
#B
#:B
#nB
#�B
#�B
$ZB
$tB
$ZB
$�B
%B
$�B
$�B
$�B
%,B
%`B
%,B
%�B
%�B
&�B
&2B
&LB
&�B
&�B
&�B
'B
'�B
'�B
(>B
'�B
'B
&B
'B
'B
&�B
'B
'8B
'B
'mB
'mB
'�B
(>B
(>B
(�B
(�B
(�B
)�B
)yB
)_B
)�B
)�B
)�B
*eB
*KB
*KB
*B
)�B
*B
*�B
*�B
*�B
*�B
*�B
*�B
+QB
+kB
+�B
+�B
,WB
+�B
+�B
+�B
,B
,"B
,WB
,qB
-B
-CB
-�B
-�B
./B
.B
-�B
.IB
/ B
/�B
0;B
0�B
1'B
0�B
0�B
0�B
0�B
1B
1[B
1vB
1vB
1�B
1vB
1vB
1[B
1'B
1AB
1B
0�B
0B
/iB
/ B
.�B
-�B
-]B
-�B
.cB
.IB
.cB
.�B
.IB
/5B
/�B
/�B
0;B
0oB
0�B
0oB
1�B
2-B
2|B
2�B
3B
2�B
3�B
4�B
4�B
5B
5?B
5�B
5�B
6�B
6�B
7B
7�B
9>B
9rB
9�B
9�B
9�B
:*B
:B
:*B
:B
:^B
:xB
;0B
;JB
;0B
:�B
:*B
:B
9�B
:DB
;B
;B
;JB
;dB
;�B
<PB
<jB
<6B
<B
<jB
<6B
<�B
<�B
=B
=�B
=�B
>B
>B
>(B
>]B
>�B
?B
?�B
?�B
?�B
?�B
@OB
@�B
@�B
A B
A�B
BB
B�B
BuB
BuB
CaB
C�B
C�B
C�B
DB
D�B
D�B
D�B
D�B
D�B
E9B
E�B
E�B
E�B
ESB
E9B
EB
E9B
E9B
E�B
F?B
F?B
FtB
FYB
FYB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
IB
IRB
I�B
JXB
JrB
JXB
JXB
J=B
J=B
JXB
J=B
J=B
JXB
JrB
J�B
KB
KxB
MB
M6B
MB
MB
MB
M�B
M�B
N"B
N"B
M�B
M�B
MB
L�B
MB
MPB
M�B
M�B
M�B
M�B
NB
NB
N�B
N�B
OB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
P.B
PHB
P�B
P�B
QNB
Q�B
Q�B
Q�B
R�B
S�B
T,B
TFB
TaB
T{B
T{B
T�B
T�B
T�B
T�B
T�B
T{B
T�B
UB
UMB
U2B
U�B
U�B
U�B
U�B
VSB
V�B
WsB
X�B
X�B
X�B
Y�B
Y�B
ZB
ZQB
ZQB
Z�B
Z�B
Z�B
[#B
[�B
\B
[�B
[�B
[�B
\B
\)B
[�B
[�B
\B
[�B
\B
\B
]B
]�B
]�B
^B
^B
]�B
^jB
^�B
^�B
^�B
^�B
_pB
_�B
_�B
_�B
`vB
`�B
abB
aHB
a-B
aHB
abB
bNB
b�B
cB
cB
dB
d&B
d@B
d&B
dZB
d�B
d�B
ezB
e�B
fLB
fLB
f�B
f�B
gRB
g�B
hsB
h�B
hXB
hsB
h�B
iDB
jB
kQB
k�B
lB
l=B
l=B
lWB
lqB
lqB
l�B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m)B
mCB
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nIB
n�B
n�B
oB
o�B
o�B
o�B
pB
p!B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
poB
qAB
qB
qB
p�B
p�B
p�B
p�B
p�B
q�B
q�B
rB
raB
raB
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sB
sB
shB
s�B
tB
tnB
t�B
t�B
t�B
u%B
u?B
u?B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220620184157  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220620184321  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220620184322  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220620184322                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220621034327  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220621034327  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220620185821                      G�O�G�O�G�O�                