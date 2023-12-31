CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-19T18:41:25Z creation;2022-12-19T18:41:27Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20221219184125  20221219185852  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���"�1   @���$i@/����o�cE����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bz  B~��B�  B�  B���B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C�C�fC	�fC  C�fC  C  C  C  C  C  C�C33C 33C!��C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CBL�CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @3�@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��By��B~p�B���B���B�k�B���B���B���B�k�B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���C��C��C�C�\C	�\C��C�\C��C��C��C��C��C��C�C)C )C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CB5�CC�\CE�\CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&��D' �D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Do �Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�#�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�҉A�خA��A���A��A�ݘA��8A��WA��WA��oA���A���A���A��(A��A�A��A�.A��A�YA�YA��A��A��A��DA��XAР�AДA�ffA��A�՛A�ĜAϩ�AΠ�A�$tA�}VA���AöFA�7�A��A���A�C-A��)A�a�A��A�`BA��$A�A� A�	lA�CA��?A��EA�[#A���A���A�|PA��A�n�A�m�A��A��A�#�A�e�A�y	A��jA�3hA�^A��A�(�A���A�gmA��A���A��BA�ɆA��A~TaAx-Aqr�Anq�AkxAj�XAh�+AcںA_��A\��AY��AR��AP�AMp;AG
=AD��AB�A>i�A<��A:�XA9��A8��A9�A8��A7dZA7xA67LA4{JA3��A3#:A2��A2�XA2�XA2�qA2ĜA2��A2VmA1��A1��A0��A0E9A08�A0T�A0�A/�-A/{�A/T�A/M�A/.�A.��A.V�A-:*A,�:A,L0A,�A+�)A+s�A*�A*VA)MjA'Q�A&oA%��A%5?A%4�A%T�A%QA% iA$�9A$҉A$��A$��A#��A"�KA"FA!��A!*0A �aA FtA �Ab�A��Ae,AX�A2aA��A� A iA��Az�AW?Ai�A�A��A �A�A��AA��A�"A!�A�PAo AL�A�A�?Au%A!�AuA�A�FA)�A	A�A�gA[�A �A��A��A��A4A��A�\Ap�A	lA�#A��Az�AM�A�"A��A3�A�A^�A��A�hAt�A(�A�#A��A�!AxA\)A
�AA>BA�YA
��A
�]A
s�A
*0A

=A	y�A	OA	�\A	;A�hA{JA�AA�A�oAP�A�<A�_AxA,�A��A�vAf�A	lA��A�AuA�6A0�A �0@�q@���@��y@��@�w�@��@�q�@���@��*@�U�@�4�@�ߤ@�c�@��5@��5@�P@�ی@�@�{@�o@�	�@�	@���@�j@�l"@�C-@�=q@�r@�YK@��@�S�@�Ĝ@ꄶ@�\�@��@��N@�x@�%@�_@�-�@� �@��@�8�@�$@�8�@��p@�S&@�z@��)@���@��@�U�@�N<@�(@�u�@�C�@�G@�{J@��x@ߩ�@�O�@��@�0U@���@�4@�[�@��@�@���@ؑ @�j�@ױ[@ךk@�S�@և�@�&�@��@�֡@�@�@�@�{@ѭC@��@��@�z@���@��@��@�%F@̐.@�1@�Y�@�Ft@��@���@ʆY@�{�@�)�@���@�6z@��@ȂA@ǖS@��@�}�@�O�@�p;@ņ�@��U@�`�@��@�C�@I@��0@�"�@��@�@�@��@�n/@�S&@���@�c�@��T@���@�[W@�C@��@���@��A@�N�@�7@���@���@�qv@�X�@�F@�5�@��s@�I�@��@��>@���@�v`@��9@�c @�Xy@�@��q@�e�@�J#@��@�i�@���@���@��p@�($@���@�:�@�8�@��3@��~@�?}@�33@��"@�u�@�@O@���@�Q@�C�@���@���@�ȴ@�[�@�	�@�� @��k@�33@��@��0@��<@�V�@�7@��@�n/@���@��B@���@��@���@�|@���@��r@�N�@��@�f�@�&�@���@�v�@�Ft@�PH@�  @��"@��N@�ƨ@���@���@�t�@�;@���@���@�6�@�ݘ@��4@��@��.@�^5@�7@��0@���@�g�@�V@���@���@�7@� �@�_@�1�@���@���@�5�@��@�l�@��@��@���@��g@��@�Mj@��|@���@�v�@�M�@��@��@��C@�~�@�C�@��E@�^5@�)�@���@���@��@��z@���@�h�@�<�@�ƨ@�=@��c@�u�@�@��t@���@�t�@�=@���@���@�Ta@��m@�t�@��@���@�m�@�R�@�O@��Q@�F@��h@���@�g8@�PH@�O@���@�o�@��@�{�@�@��w@��@@�N<@�$t@��@���@��\@�7@��@��@��#@���@�j�@�=@���@��O@�?�@�"h@��'@�K�@�IR@�=�@���@���@��b@���@�u�@�Z�@���@���@��U@���@�?�@�	@�b@��@��+@�ݘ@�j�@��@��!@��Y@�z�@�H�@�*�@�{@ݘ@��@4�@~��@~a|@}�3@|�@{�:@{U�@{,�@z��@zv�@zM�@zR�@zTa@y�-@x�@w�a@w,�@v�y@v��@v�h@vkQ@u�@u;@tZ@t1@s\)@r}V@rYK@rOv@r=q@r�@q�N@q/@p�.@o��@oU�@n�"@nd�@nOv@nJ@m�@m�M@mA @m�@l�`@lXy@k��@ko@j�s@j�\@j�A@j:*@i��@i��@i}�@iF@h�@h�Y@h7@h�@g˒@gs@f��@e��@ek�@e \@d|�@d�@c��@cK�@b��@bTa@a��@a�@a�@`��@`*�@_��@_@^�<@^��@^�@^Z�@^e@]�3@]rG@]`B@\�f@\S�@[�@[RT@[/�@[@Z��@Z^5@Z&�@Y�j@Y�@X��@X�@Xj@X*�@Wݘ@W�@W�{@W;d@WC@V�]@V�b@V8�@U�Z@U��@U��@U-w@U�@T��@Tj@S�6@S�4@S(@R��@R)�@Q��@Q�)@Q��@QG�@Q+@P��@P�@Pc�@O��@O4�@N�L@Nc @N=q@Ne@M�-@M+�@M;@L��@L��@L�?@L>B@Kخ@K��@K4�@J�@J\�@JJ@I�X@I(�@H�e@HXy@H6@H@G�m@Gƨ@G�$@GdZ@G$t@F�@Fc @Fff@E�.@Ex�@E@@D��@D�@C��@C�*@Cqv@Co@B�b@Bl�@Bh
@BOv@A��@AO�@A@@@��@@��@@z�@@C-@@%�@?�[@?_p@?�@>�@>{�@>R�@>�@=�t@=�@<�v@<�U@<�j@<�9@<�u@<S�@< �@;�W@;��@;RT@:��@:��@:E�@9��@9�N@9�S@9IR@9�@8�K@8�O@8Q�@7��@7'�@6�s@6��@6��@6!�@5�T@5�H@5��@5��@5N<@4��@4Ĝ@4�$@4q@4%�@4G@3t�@3�@2~�@2Ta@2{@1��@1�t@1�'@1c@1N<@1@1�@0�@0��@0h�@0(�@/�g@/�$@/o�@/>�@/Y@.�2@.��@.�\@._�@.=q@.0U@-�.@-�o@-��@-�7@-G�@,��@,�_@,c�@,!@+��@+_p@+@*��@*a|@*
�@)��@)k�@)4@(�@(֡@(�@(��@(e�@(7�@(�@'��@'�[@'��@'W?@'�@&�s@&�@&($@%��@%��@%m]@%c�@%Vm@%B�@%2a@%/@%(�@%+@$��@$�j@$c�@$9X@$@#�@#�K@#�[@#~�@#>�@#@"�h@"ff@"�@!�>@!�@!�n@!x�@!5�@ �|@ �@ PH@ A�@ �@��@�}@�K@n/@ߤ@�@s�@0U@�@u�@�@��@�_@A�@'R@�]@�Q@��@��@{J@E9@&@�@u%@L0@@�3@m]@[W@(�@�@��@]d@~@�;@�q@��@\)@/�@Y@�@��@��@�b@YK@u@�#@��@��@�=@w2@/@��@��@��@V�@�@��@�&@�g@˒@��@��@g�@,�@�2@��@��@q�@YK@)�@�#@�t@��@4@@�9@�D@�o@j@:�@'R@x@�
@�F@��@��@U�@@O@/�@.I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�҉A�خA��A���A��A�ݘA��8A��WA��WA��oA���A���A���A��(A��A�A��A�.A��A�YA�YA��A��A��A��DA��XAР�AДA�ffA��A�՛A�ĜAϩ�AΠ�A�$tA�}VA���AöFA�7�A��A���A�C-A��)A�a�A��A�`BA��$A�A� A�	lA�CA��?A��EA�[#A���A���A�|PA��A�n�A�m�A��A��A�#�A�e�A�y	A��jA�3hA�^A��A�(�A���A�gmA��A���A��BA�ɆA��A~TaAx-Aqr�Anq�AkxAj�XAh�+AcںA_��A\��AY��AR��AP�AMp;AG
=AD��AB�A>i�A<��A:�XA9��A8��A9�A8��A7dZA7xA67LA4{JA3��A3#:A2��A2�XA2�XA2�qA2ĜA2��A2VmA1��A1��A0��A0E9A08�A0T�A0�A/�-A/{�A/T�A/M�A/.�A.��A.V�A-:*A,�:A,L0A,�A+�)A+s�A*�A*VA)MjA'Q�A&oA%��A%5?A%4�A%T�A%QA% iA$�9A$҉A$��A$��A#��A"�KA"FA!��A!*0A �aA FtA �Ab�A��Ae,AX�A2aA��A� A iA��Az�AW?Ai�A�A��A �A�A��AA��A�"A!�A�PAo AL�A�A�?Au%A!�AuA�A�FA)�A	A�A�gA[�A �A��A��A��A4A��A�\Ap�A	lA�#A��Az�AM�A�"A��A3�A�A^�A��A�hAt�A(�A�#A��A�!AxA\)A
�AA>BA�YA
��A
�]A
s�A
*0A

=A	y�A	OA	�\A	;A�hA{JA�AA�A�oAP�A�<A�_AxA,�A��A�vAf�A	lA��A�AuA�6A0�A �0@�q@���@��y@��@�w�@��@�q�@���@��*@�U�@�4�@�ߤ@�c�@��5@��5@�P@�ی@�@�{@�o@�	�@�	@���@�j@�l"@�C-@�=q@�r@�YK@��@�S�@�Ĝ@ꄶ@�\�@��@��N@�x@�%@�_@�-�@� �@��@�8�@�$@�8�@��p@�S&@�z@��)@���@��@�U�@�N<@�(@�u�@�C�@�G@�{J@��x@ߩ�@�O�@��@�0U@���@�4@�[�@��@�@���@ؑ @�j�@ױ[@ךk@�S�@և�@�&�@��@�֡@�@�@�@�{@ѭC@��@��@�z@���@��@��@�%F@̐.@�1@�Y�@�Ft@��@���@ʆY@�{�@�)�@���@�6z@��@ȂA@ǖS@��@�}�@�O�@�p;@ņ�@��U@�`�@��@�C�@I@��0@�"�@��@�@�@��@�n/@�S&@���@�c�@��T@���@�[W@�C@��@���@��A@�N�@�7@���@���@�qv@�X�@�F@�5�@��s@�I�@��@��>@���@�v`@��9@�c @�Xy@�@��q@�e�@�J#@��@�i�@���@���@��p@�($@���@�:�@�8�@��3@��~@�?}@�33@��"@�u�@�@O@���@�Q@�C�@���@���@�ȴ@�[�@�	�@�� @��k@�33@��@��0@��<@�V�@�7@��@�n/@���@��B@���@��@���@�|@���@��r@�N�@��@�f�@�&�@���@�v�@�Ft@�PH@�  @��"@��N@�ƨ@���@���@�t�@�;@���@���@�6�@�ݘ@��4@��@��.@�^5@�7@��0@���@�g�@�V@���@���@�7@� �@�_@�1�@���@���@�5�@��@�l�@��@��@���@��g@��@�Mj@��|@���@�v�@�M�@��@��@��C@�~�@�C�@��E@�^5@�)�@���@���@��@��z@���@�h�@�<�@�ƨ@�=@��c@�u�@�@��t@���@�t�@�=@���@���@�Ta@��m@�t�@��@���@�m�@�R�@�O@��Q@�F@��h@���@�g8@�PH@�O@���@�o�@��@�{�@�@��w@��@@�N<@�$t@��@���@��\@�7@��@��@��#@���@�j�@�=@���@��O@�?�@�"h@��'@�K�@�IR@�=�@���@���@��b@���@�u�@�Z�@���@���@��U@���@�?�@�	@�b@��@��+@�ݘ@�j�@��@��!@��Y@�z�@�H�@�*�@�{@ݘ@��@4�@~��@~a|@}�3@|�@{�:@{U�@{,�@z��@zv�@zM�@zR�@zTa@y�-@x�@w�a@w,�@v�y@v��@v�h@vkQ@u�@u;@tZ@t1@s\)@r}V@rYK@rOv@r=q@r�@q�N@q/@p�.@o��@oU�@n�"@nd�@nOv@nJ@m�@m�M@mA @m�@l�`@lXy@k��@ko@j�s@j�\@j�A@j:*@i��@i��@i}�@iF@h�@h�Y@h7@h�@g˒@gs@f��@e��@ek�@e \@d|�@d�@c��@cK�@b��@bTa@a��@a�@a�@`��@`*�@_��@_@^�<@^��@^�@^Z�@^e@]�3@]rG@]`B@\�f@\S�@[�@[RT@[/�@[@Z��@Z^5@Z&�@Y�j@Y�@X��@X�@Xj@X*�@Wݘ@W�@W�{@W;d@WC@V�]@V�b@V8�@U�Z@U��@U��@U-w@U�@T��@Tj@S�6@S�4@S(@R��@R)�@Q��@Q�)@Q��@QG�@Q+@P��@P�@Pc�@O��@O4�@N�L@Nc @N=q@Ne@M�-@M+�@M;@L��@L��@L�?@L>B@Kخ@K��@K4�@J�@J\�@JJ@I�X@I(�@H�e@HXy@H6@H@G�m@Gƨ@G�$@GdZ@G$t@F�@Fc @Fff@E�.@Ex�@E@@D��@D�@C��@C�*@Cqv@Co@B�b@Bl�@Bh
@BOv@A��@AO�@A@@@��@@��@@z�@@C-@@%�@?�[@?_p@?�@>�@>{�@>R�@>�@=�t@=�@<�v@<�U@<�j@<�9@<�u@<S�@< �@;�W@;��@;RT@:��@:��@:E�@9��@9�N@9�S@9IR@9�@8�K@8�O@8Q�@7��@7'�@6�s@6��@6��@6!�@5�T@5�H@5��@5��@5N<@4��@4Ĝ@4�$@4q@4%�@4G@3t�@3�@2~�@2Ta@2{@1��@1�t@1�'@1c@1N<@1@1�@0�@0��@0h�@0(�@/�g@/�$@/o�@/>�@/Y@.�2@.��@.�\@._�@.=q@.0U@-�.@-�o@-��@-�7@-G�@,��@,�_@,c�@,!@+��@+_p@+@*��@*a|@*
�@)��@)k�@)4@(�@(֡@(�@(��@(e�@(7�@(�@'��@'�[@'��@'W?@'�@&�s@&�@&($@%��@%��@%m]@%c�@%Vm@%B�@%2a@%/@%(�@%+@$��@$�j@$c�@$9X@$@#�@#�K@#�[@#~�@#>�@#@"�h@"ff@"�@!�>@!�@!�n@!x�@!5�@ �|@ �@ PH@ A�@ �@��@�}@�K@n/@ߤ@�@s�@0U@�@u�@�@��@�_@A�@'R@�]@�Q@��@��@{J@E9@&@�@u%@L0@@�3@m]@[W@(�@�@��@]d@~@�;@�q@��@\)@/�@Y@�@��@��@�b@YK@u@�#@��@��@�=@w2@/@��@��@��@V�@�@��@�&@�g@˒@��@��@g�@,�@�2@��@��@q�@YK@)�@�#@�t@��@4@@�9@�D@�o@j@:�@'R@x@�
@�F@��@��@U�@@O@/�@.I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
)yB
)B
($B
)_B
+B
*eB
)�B
+�B
-B
,�B
.}B
0oB
/ B
1�B
4B
7�B
:xB
>BB
AUB
C{B
D�B
E�B
F�B
I�B
LB
J�B
J	B
I�B
J	B
K�B
NpB
N�B
P.B
Q�B
X�B
\xB
lB
r�B
~�B
oB
��BF�B��B��BxB>�BA�B]�Bb�Bl"B~�B~(BxBzB��B�6B��Bt�B�_B��BshBc B`'B[=BR�B?�B
�NB
��B
� B
WYB
IlB
)*B
�B
�B	��B	�B	ߤB	�\B	��B	�xB	�B	��B	~�B	}qB	{0B	m]B	\�B	RB	D�B	#nB	@B	
�B��B��B�B�yB��B	�B		7B	)B	�B	{B	-B	4B	G�B	_pB	lB	�	B	�=B	�vB	�@B	�B	��B	�:B	�eB	��B	�_B	��B	�kB	�HB	�B
gB
(B
�B
yB
xB
!B
%,B
'�B
)*B
*�B
0B
4TB
5ZB
6+B
5B
1[B
/iB
(
B
$�B
#�B
%B
)B
,�B
1AB
6zB
9	B
:�B
<�B
=qB
<�B
6FB
88B
72B
6`B
6�B
4B
3MB
2|B
/�B
0�B
4�B
;�B
;dB
8�B
3�B
3hB
2�B
4B
9�B
=<B
>B
E�B
L~B
N�B
O�B
O�B
PB
PB
PB
Q4B
Q�B
Q�B
R�B
S�B
T,B
S�B
S�B
TB
TFB
TaB
TB
TB
T�B
S�B
T�B
T�B
S�B
TB
R�B
R�B
R�B
Q�B
PHB
PB
O�B
O�B
N�B
NB
L�B
L�B
KDB
JrB
I7B
H�B
G�B
FYB
D�B
AoB
=�B
:�B
8lB
<�B
D�B
E�B
K�B
N�B
KxB
JXB
G�B
C�B
M�B
MB
K)B
I�B
OB
KDB
JXB
K^B
IRB
J=B
I�B
HfB
F�B
DB
A�B
?cB
=B
8�B
0�B
1�B
/�B
+�B
&fB
!�B
�B
~B
�B
�B
aB
B
1B
B
�B
�B
�B
9B
�B
�B
SB
�B
uB
;B
 �B
mB
B
 �B
 B
�B
AB
�B
MB
�B
B
�B
gB
�B
�B
B
�B
�B
�B
B
 �B	��B	�B	�B	�BB
UB
�B
�B
GB
B
%B

�B
�B
0B
xB
�B
�B
�B
0B
DB

=B

	B
	B
�B
�B
B
�B
�B	��B	��B	�B	�B
  B	��B	�PB	�RB	�B	�B	��B	��B	�|B	�B	��B	�*B	��B	�B	�2B	��B	�2B	��B	�?B	�B	�B	�B	��B	�fB	��B	�$B	�xB	�0B	�B	�*B	�LB	�+B	��B	�PB	��B	�0B	��B	�DB	�rB	�XB	�RB	�B	�RB	�B	�>B	�^B	�DB	��B	��B	��B	�B	�DB	��B	�B	�B	��B	��B	�dB	�B	�B	�B	�6B	��B	�HB	�B	�cB
 4B
 �B
 OB
 4B
 iB
;B
;B
oB
�B
oB
�B
�B
uB
�B
uB
AB
�B
-B
AB
aB
B
aB
�B
�B
3B
YB
�B
SB
9B
�B
%B
B
�B
�B
�B
�B
�B
+B
�B
�B
�B
	7B
�B
�B
	B
	�B
	�B

	B
	�B
	�B
	7B

=B
	�B
	�B
	�B
	7B
	RB
	�B
	B
	B
	B

�B
)B

�B
<B
"B
�B
B
bB
bB
 B
B
.B
�B
BB
�B
"B
VB
B
PB
�B
�B
�B
�B
�B
VB
�B
�B
FB
�B
gB
�B
�B
FB
{B
MB
�B
�B
B
+B
YB
sB
EB
�B
�B
�B
	B
�B
�B
B
�B
CB
�B
�B
~B
B
�B
�B
�B
pB
 vB
 BB
!-B
!HB
!|B
!|B
!|B
!�B
!�B
!�B
!bB
 �B
!bB
"�B
#TB
#nB
#nB
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
&2B
&fB
'B
'B
&B
%�B
%`B
%�B
%FB
$�B
$tB
$�B
&�B
'B
'B
&�B
'B
'�B
(sB
*0B
+B
+�B
,B
-CB
-�B
-�B
.B
.}B
/ B
/B
/B
/5B
.�B
-wB
+�B
,=B
.IB
.B
.�B
.�B
.�B
/ B
.�B
.�B
.�B
.�B
.IB
./B
.�B
/�B
/�B
0;B
/�B
0B
1'B
1�B
0�B
0B
/iB
/�B
/B
/B
.�B
/B
/�B
0�B
0�B
1AB
0�B
0�B
0oB
0oB
0UB
0oB
1AB
0�B
0�B
0�B
1B
1�B
1�B
2B
2�B
33B
4B
4nB
49B
4�B
5B
5�B
6�B
7�B
7�B
7�B
7fB
7�B
8B
8RB
8�B
9�B
:B
:DB
;B
;B
;�B
;�B
;�B
;�B
;�B
<B
<�B
<�B
<�B
=<B
=VB
=�B
=�B
=qB
=�B
=�B
>�B
>�B
>BB
>�B
?cB
@4B
@�B
A�B
B'B
BuB
B�B
B�B
B�B
B�B
CGB
C�B
DB
D�B
D�B
D�B
EB
E�B
F�B
FtB
FtB
F�B
F�B
G+B
GEB
GEB
HB
H�B
HfB
H�B
H�B
IB
IB
I7B
IlB
IlB
I�B
I�B
J=B
J=B
JrB
J�B
J�B
J�B
J�B
KDB
K�B
K�B
LJB
L�B
MB
M6B
MB
M6B
MjB
M�B
M�B
M�B
N<B
NVB
O(B
O�B
O�B
O�B
O�B
P.B
P�B
P�B
P}B
P}B
P}B
QNB
QB
Q4B
Q�B
RB
R:B
R�B
R�B
SB
SuB
S�B
T,B
TaB
T{B
T�B
T�B
T�B
T�B
UgB
UgB
U2B
U�B
U�B
VB
V�B
W$B
WYB
WsB
W�B
X_B
X�B
YeB
YeB
YKB
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
[�B
[�B
[�B
[�B
\CB
\�B
\�B
]B
]dB
]dB
]�B
]�B
]�B
^B
^5B
^OB
^�B
^�B
_!B
_VB
_�B
_�B
_�B
`BB
`\B
`vB
`�B
`�B
a�B
a�B
b4B
b4B
bNB
b�B
b�B
b�B
cB
cB
c:B
c�B
c�B
c�B
dB
d&B
d&B
d�B
eB
ezB
ezB
e�B
e�B
e�B
e�B
e�B
fLB
fLB
ffB
f�B
f�B
gB
gB
g�B
g�B
g�B
g�B
h
B
h$B
hXB
hXB
h�B
h�B
h�B
h�B
h�B
iB
iB
i_B
i�B
i�B
i�B
jKB
j�B
j�B
kB
kQB
kkB
k�B
l=B
l=B
l�B
l�B
l�B
l�B
l�B
m)B
mwB
mwB
m�B
m�B
m�B
nIB
ncB
n}B
n�B
n�B
oB
n�B
oB
oB
o B
o5B
oOB
o5B
oOB
oOB
oOB
o�B
pB
pB
p;B
pUB
p�B
p�B
qB
qAB
qAB
qvB
q�B
rGB
r|B
r�B
r�B
r�B
s3B
s3B
shB
s�B
s�B
s�B
tB
tB
tB
t�B
u%B
u?B
utB
u�B
vB
vFB
v�B
v�B
wB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
x8B
xRB
x�B
y	B
yXB
y�B
y�B
y�B
z*B
zDB
z�B
z�B
{0B
{dB
{B
{B
{�B
{�B
|B
{�B
|B
|B
|PB
|�B
|�B
}B
}"B
}"B
}"B
}<B
}�B
}�B
}�B
~B
~BB
~�B
~wB
~�B
~�B
~�B
~�B
~�B
.B
.B
}B
�B
�B
�B
�B
� B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
�uB
��B
��B
��B
�B
�GB
�GB
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
)yB
)B
($B
)_B
+B
*eB
)�B
+�B
-B
,�B
.}B
0oB
/ B
1�B
4B
7�B
:xB
>BB
AUB
C{B
D�B
E�B
F�B
I�B
LB
J�B
J	B
I�B
J	B
K�B
NpB
N�B
P.B
Q�B
X�B
\xB
lB
r�B
~�B
oB
��BF�B��B��BxB>�BA�B]�Bb�Bl"B~�B~(BxBzB��B�6B��Bt�B�_B��BshBc B`'B[=BR�B?�B
�NB
��B
� B
WYB
IlB
)*B
�B
�B	��B	�B	ߤB	�\B	��B	�xB	�B	��B	~�B	}qB	{0B	m]B	\�B	RB	D�B	#nB	@B	
�B��B��B�B�yB��B	�B		7B	)B	�B	{B	-B	4B	G�B	_pB	lB	�	B	�=B	�vB	�@B	�B	��B	�:B	�eB	��B	�_B	��B	�kB	�HB	�B
gB
(B
�B
yB
xB
!B
%,B
'�B
)*B
*�B
0B
4TB
5ZB
6+B
5B
1[B
/iB
(
B
$�B
#�B
%B
)B
,�B
1AB
6zB
9	B
:�B
<�B
=qB
<�B
6FB
88B
72B
6`B
6�B
4B
3MB
2|B
/�B
0�B
4�B
;�B
;dB
8�B
3�B
3hB
2�B
4B
9�B
=<B
>B
E�B
L~B
N�B
O�B
O�B
PB
PB
PB
Q4B
Q�B
Q�B
R�B
S�B
T,B
S�B
S�B
TB
TFB
TaB
TB
TB
T�B
S�B
T�B
T�B
S�B
TB
R�B
R�B
R�B
Q�B
PHB
PB
O�B
O�B
N�B
NB
L�B
L�B
KDB
JrB
I7B
H�B
G�B
FYB
D�B
AoB
=�B
:�B
8lB
<�B
D�B
E�B
K�B
N�B
KxB
JXB
G�B
C�B
M�B
MB
K)B
I�B
OB
KDB
JXB
K^B
IRB
J=B
I�B
HfB
F�B
DB
A�B
?cB
=B
8�B
0�B
1�B
/�B
+�B
&fB
!�B
�B
~B
�B
�B
aB
B
1B
B
�B
�B
�B
9B
�B
�B
SB
�B
uB
;B
 �B
mB
B
 �B
 B
�B
AB
�B
MB
�B
B
�B
gB
�B
�B
B
�B
�B
�B
B
 �B	��B	�B	�B	�BB
UB
�B
�B
GB
B
%B

�B
�B
0B
xB
�B
�B
�B
0B
DB

=B

	B
	B
�B
�B
B
�B
�B	��B	��B	�B	�B
  B	��B	�PB	�RB	�B	�B	��B	��B	�|B	�B	��B	�*B	��B	�B	�2B	��B	�2B	��B	�?B	�B	�B	�B	��B	�fB	��B	�$B	�xB	�0B	�B	�*B	�LB	�+B	��B	�PB	��B	�0B	��B	�DB	�rB	�XB	�RB	�B	�RB	�B	�>B	�^B	�DB	��B	��B	��B	�B	�DB	��B	�B	�B	��B	��B	�dB	�B	�B	�B	�6B	��B	�HB	�B	�cB
 4B
 �B
 OB
 4B
 iB
;B
;B
oB
�B
oB
�B
�B
uB
�B
uB
AB
�B
-B
AB
aB
B
aB
�B
�B
3B
YB
�B
SB
9B
�B
%B
B
�B
�B
�B
�B
�B
+B
�B
�B
�B
	7B
�B
�B
	B
	�B
	�B

	B
	�B
	�B
	7B

=B
	�B
	�B
	�B
	7B
	RB
	�B
	B
	B
	B

�B
)B

�B
<B
"B
�B
B
bB
bB
 B
B
.B
�B
BB
�B
"B
VB
B
PB
�B
�B
�B
�B
�B
VB
�B
�B
FB
�B
gB
�B
�B
FB
{B
MB
�B
�B
B
+B
YB
sB
EB
�B
�B
�B
	B
�B
�B
B
�B
CB
�B
�B
~B
B
�B
�B
�B
pB
 vB
 BB
!-B
!HB
!|B
!|B
!|B
!�B
!�B
!�B
!bB
 �B
!bB
"�B
#TB
#nB
#nB
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
&2B
&fB
'B
'B
&B
%�B
%`B
%�B
%FB
$�B
$tB
$�B
&�B
'B
'B
&�B
'B
'�B
(sB
*0B
+B
+�B
,B
-CB
-�B
-�B
.B
.}B
/ B
/B
/B
/5B
.�B
-wB
+�B
,=B
.IB
.B
.�B
.�B
.�B
/ B
.�B
.�B
.�B
.�B
.IB
./B
.�B
/�B
/�B
0;B
/�B
0B
1'B
1�B
0�B
0B
/iB
/�B
/B
/B
.�B
/B
/�B
0�B
0�B
1AB
0�B
0�B
0oB
0oB
0UB
0oB
1AB
0�B
0�B
0�B
1B
1�B
1�B
2B
2�B
33B
4B
4nB
49B
4�B
5B
5�B
6�B
7�B
7�B
7�B
7fB
7�B
8B
8RB
8�B
9�B
:B
:DB
;B
;B
;�B
;�B
;�B
;�B
;�B
<B
<�B
<�B
<�B
=<B
=VB
=�B
=�B
=qB
=�B
=�B
>�B
>�B
>BB
>�B
?cB
@4B
@�B
A�B
B'B
BuB
B�B
B�B
B�B
B�B
CGB
C�B
DB
D�B
D�B
D�B
EB
E�B
F�B
FtB
FtB
F�B
F�B
G+B
GEB
GEB
HB
H�B
HfB
H�B
H�B
IB
IB
I7B
IlB
IlB
I�B
I�B
J=B
J=B
JrB
J�B
J�B
J�B
J�B
KDB
K�B
K�B
LJB
L�B
MB
M6B
MB
M6B
MjB
M�B
M�B
M�B
N<B
NVB
O(B
O�B
O�B
O�B
O�B
P.B
P�B
P�B
P}B
P}B
P}B
QNB
QB
Q4B
Q�B
RB
R:B
R�B
R�B
SB
SuB
S�B
T,B
TaB
T{B
T�B
T�B
T�B
T�B
UgB
UgB
U2B
U�B
U�B
VB
V�B
W$B
WYB
WsB
W�B
X_B
X�B
YeB
YeB
YKB
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
[�B
[�B
[�B
[�B
\CB
\�B
\�B
]B
]dB
]dB
]�B
]�B
]�B
^B
^5B
^OB
^�B
^�B
_!B
_VB
_�B
_�B
_�B
`BB
`\B
`vB
`�B
`�B
a�B
a�B
b4B
b4B
bNB
b�B
b�B
b�B
cB
cB
c:B
c�B
c�B
c�B
dB
d&B
d&B
d�B
eB
ezB
ezB
e�B
e�B
e�B
e�B
e�B
fLB
fLB
ffB
f�B
f�B
gB
gB
g�B
g�B
g�B
g�B
h
B
h$B
hXB
hXB
h�B
h�B
h�B
h�B
h�B
iB
iB
i_B
i�B
i�B
i�B
jKB
j�B
j�B
kB
kQB
kkB
k�B
l=B
l=B
l�B
l�B
l�B
l�B
l�B
m)B
mwB
mwB
m�B
m�B
m�B
nIB
ncB
n}B
n�B
n�B
oB
n�B
oB
oB
o B
o5B
oOB
o5B
oOB
oOB
oOB
o�B
pB
pB
p;B
pUB
p�B
p�B
qB
qAB
qAB
qvB
q�B
rGB
r|B
r�B
r�B
r�B
s3B
s3B
shB
s�B
s�B
s�B
tB
tB
tB
t�B
u%B
u?B
utB
u�B
vB
vFB
v�B
v�B
wB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
x8B
xRB
x�B
y	B
yXB
y�B
y�B
y�B
z*B
zDB
z�B
z�B
{0B
{dB
{B
{B
{�B
{�B
|B
{�B
|B
|B
|PB
|�B
|�B
}B
}"B
}"B
}"B
}<B
}�B
}�B
}�B
~B
~BB
~�B
~wB
~�B
~�B
~�B
~�B
~�B
.B
.B
}B
�B
�B
�B
�B
� B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
�uB
��B
��B
��B
�B
�GB
�GB
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221219184123  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221219184125  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221219184126  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221219184127                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221219184127  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221219184127  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221219185852                      G�O�G�O�G�O�                