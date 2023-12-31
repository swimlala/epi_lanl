CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:37:26Z creation;2022-06-04T17:37:27Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173726  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               RA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�r]So�1   @�r]��d@.�\(���c� ě��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB�  B���B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  C   C�fC  C  C  C
  C  C  C  C  C�C�C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct33Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� DfD� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bp
=Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BȞ�B�8RB���BӞ�B���B���B���B���B���B���B�B�8RB���B���B���C�\C��C��C��C	��C��C��C��C��C�C�C��C��C�\C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CJ�CL�CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Ct)Cu�\Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=D��D �Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D* �D*z=D*�=D+z=D+�=D,z=D,�=D-s�D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DX��DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D �Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$@A�$@A�$@A�#:A�#�A�#nA�~A��A�#:A�A��A��A˖�A�;A�^�A�H�A�~A���A���AɵAɞ�AɈ�A�v`A�2�A�w�A���A��A���A�ĜAǽ<AǯAǓ�A�}�A�<�A��A���AƤ�AƌJA�\�A�'RA�� Aŷ�AŌA�d�A�d�AŔ�A��dA���A�^�A�A��A�$A��A�O�A�i�A��5A��A��A�oA�YA���A���A��A�'A�s�A���A���A��;A�W�A�.}A�J�A�'A�g�A���A�poA���A��A���A�0!A��7A�sA��A��,A���A�XEA�{�A�4A�@�A���A���A���A���A��sA�k�A��A��KA���A�Q�A��
A�v`A�� A�r�A�T�A��lA���A�&A��A�M�A���A��A~kQAzDgAtAjt�A_ɆAVOvAR�AM��AKVmAJ�AG�ACc�ABA�A@��A?	A=ϫA;��A:\�A:=�A9��A8ݘA8+kA7��A7I�A6��A4�HA4�MA6�HA6�hA6)_A5'�A5g�A4��A3��A2_A1��A1��A1PHA0�xA/Y�A.��A.HA.xA-��A-C�A,�AA,�jA,1'A+qA+;�A+%A*��A*+A)�A(  A'��A'��A'o�A&��A%��A%�<A$�HA#҉A"�LA ��A	�A�bAN<A��A7LA��A_pAA@A�A��A�#A/A��A��Av`A�!A��A!�AR�A�AIRA-A�}A)_A��A[�AMjAE�A�9ArGA+kA�A�NA��A�A�Aj�A.IA��AS�A��AxA	lA�Ao A�+A�DAc�AA�A�A
�A
��A
c A
C�A
-A	�A	�2A	ĜA	w�A	V�A�~AqvAMA�DA�A��A�Ae,AJ�A��A�Ad�A �A��A{A�HA<6A�)A�oA<6A�A�A�_AW�A�A�pA4A ��A IR@��@�(@���@��@�+k@�<6@���@��)@�kQ@�"@��@�A@�O@�@�@��@�F�@�w�@��@�g�@�\@�^5@�@�l"@��N@딯@���@�C@��@�@�(�@繌@�J�@�.@�D�@�-@��5@䎊@㋬@�r�@���@��@��@�w2@ऩ@��@���@�[W@�@܃�@��#@�E9@ڶ�@�`�@�v`@���@؍�@�)_@���@�j�@Է�@�b@�,=@�,=@��W@Ӻ^@�l�@��B@ѕ�@��9@μj@��@̑�@ʚ�@Ɇ�@��@���@�O@��@Ǐ�@�k�@�N<@��@�xl@�~@�  @��9@ŕ�@�H�@�;@��@�<6@���@���@F@�@��[@�n/@�%F@�p;@��d@�rG@�
=@�_�@�9X@�u@�b�@���@�:�@���@�*0@���@��<@���@�H�@���@�[W@�P�@��s@��t@�%@��_@�4n@���@��f@���@�&�@��T@�t�@�A�@�ی@�	@���@�v`@�f�@���@�M�@�,=@���@�e,@��$@��+@�Z�@�	@���@�[W@�%@���@��/@�Ĝ@���@���@�#:@��@�ƨ@��@���@�F@�33@���@��@���@�Q@���@�RT@�8�@�n/@�t�@�6z@�Mj@�q@��@�X@��@�C-@�PH@��@�خ@��P@���@�S�@��=@�X�@��B@���@�s�@��>@���@��h@�e�@��5@��L@���@�H�@��@��@�ԕ@��3@�zx@�7L@�C@��@���@�V@���@��@���@��Z@�{J@�֡@��X@���@���@�\�@�@�@�x@��@��	@�|�@�zx@�hs@�ѷ@��$@��@�YK@�'R@��@�x�@�@@��$@�g8@�K^@��@���@�f�@�=@�9�@��@�{�@�1'@���@���@���@�Vm@�#�@���@��.@�d�@�,=@�	�@���@��n@�y�@�s�@�Vm@�?}@��[@�z@�6�@�x@��@�y�@��@�p;@��@���@���@�{J@�1�@���@�Q�@�;�@��@�j�@��@���@���@�6@�_@��=@�8�@�"�@��@���@�~(@�j@�4@���@�a@�q@���@��u@�p;@�[�@�/�@���@��!@���@�YK@��@��K@�T�@��@�+@��@��@���@�u%@�V@�2�@�u@��w@���@�P�@�*0@���@�g8@�R�@�b@�@~��@}��@|��@|,=@|@|�@{خ@{j�@{"�@z͟@zGE@y��@y^�@yf�@yzx@yIR@x��@xD�@wخ@w33@v҉@v��@v�h@v�1@u��@t�@tH@t$@s��@r�X@r�h@rz@q�@qVm@p�@pu�@o��@o��@o8@n��@m�@m%@ltT@lPH@l  @kg�@k33@k i@j�}@j{�@i��@iY�@h��@g��@g�@f��@f�+@fOv@e�o@e�#@ex�@e0�@dh�@c�w@cdZ@b��@a�@a�t@ac@aG�@`�	@`�@`*�@`M@_�@_b�@_�@^��@^u%@^E�@^
�@]�@]��@]B�@\�@\m�@[�&@Z��@Ze@Y`B@Y+@X��@X/�@W�Q@W��@Wv`@WA�@V�s@V!�@U�@Uc�@U@T�@T>B@SO@S$t@S i@R�}@R}V@Rc @R?@R�@Q�n@QDg@P�@P��@P��@Pq@PM@O�q@Oa@OW?@OJ#@N��@N��@N�A@NYK@NE�@N@M�3@M`B@Lm�@K��@K��@KJ#@J��@J��@JGE@I \@Hr�@G��@G|�@GX�@F��@F��@F��@FB[@FJ@E�9@E�h@E<6@E�@D��@D��@D��@D:�@C�P@C�@B�R@B��@Bd�@BE�@A�)@A}�@AVm@A5�@@�@@Z@?�&@?y�@?/�@>҉@>�b@>kQ@>�@=�t@=�~@=<6@<�v@<z�@<<�@<"h@;��@;!-@:�}@:��@:�@:@9�-@9s�@9J�@9!�@8ی@8I�@8'R@7�]@7�*@7P�@7�@6�@6��@6Ov@68�@6$�@6e@5�@5�@5�-@5��@5�~@5N<@5�@4�j@4r�@4j@4PH@46@3�r@3��@3{J@3e�@39�@3�@2ff@1�>@1s�@1�@0�@0h�@0b@/�g@/t�@/J#@/4�@.�"@.��@.W�@-��@-�h@-Q�@-�@,֡@,�$@,r�@,�@+��@+j�@+�@*�R@*�A@*Z�@*+k@*	@)�@)�@)�-@)k�@)/@(��@(S�@(�@'�&@'��@'��@'��@'�:@'g�@'K�@'@&�@&�@&��@&;�@%�@%j@$�f@$�4@$[�@$-�@$  @#�F@#{J@#6z@#�@"�@"p;@"a|@"@!�#@!�h@!O�@!@!	l@ �`@ �p@ ��@ l"@ @�a@��@4�@��@��@�@��@��@*0@�v@�I@bN@Ft@�@�*@g�@�c@V@�@��@�'@f�@A @�@�p@�D@PH@@  @�@��@��@��@v`@W?@,�@��@� @��@�@�9@�@F@B�@G�@5�@V@�@��@�@�/@�@l"@c�@[�@?�@�@��@��@�Q@��@��@�f@|�@F�@Y@ i@��@��@�1@kQ@�@�T@�9@��@��@�"@zx@e,@S&@q@��@�@�?@�z@��@�.@�D@bN@>B@*�@"h@�@�&@�g@�K@�0@�V@��@W?@8@!-@�@�@ں@�}@�F@~�@i�@E�@3�@&�@�@��@�9@�C@��@s�@Dg@V@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$@A�$@A�$@A�#:A�#�A�#nA�~A��A�#:A�A��A��A˖�A�;A�^�A�H�A�~A���A���AɵAɞ�AɈ�A�v`A�2�A�w�A���A��A���A�ĜAǽ<AǯAǓ�A�}�A�<�A��A���AƤ�AƌJA�\�A�'RA�� Aŷ�AŌA�d�A�d�AŔ�A��dA���A�^�A�A��A�$A��A�O�A�i�A��5A��A��A�oA�YA���A���A��A�'A�s�A���A���A��;A�W�A�.}A�J�A�'A�g�A���A�poA���A��A���A�0!A��7A�sA��A��,A���A�XEA�{�A�4A�@�A���A���A���A���A��sA�k�A��A��KA���A�Q�A��
A�v`A�� A�r�A�T�A��lA���A�&A��A�M�A���A��A~kQAzDgAtAjt�A_ɆAVOvAR�AM��AKVmAJ�AG�ACc�ABA�A@��A?	A=ϫA;��A:\�A:=�A9��A8ݘA8+kA7��A7I�A6��A4�HA4�MA6�HA6�hA6)_A5'�A5g�A4��A3��A2_A1��A1��A1PHA0�xA/Y�A.��A.HA.xA-��A-C�A,�AA,�jA,1'A+qA+;�A+%A*��A*+A)�A(  A'��A'��A'o�A&��A%��A%�<A$�HA#҉A"�LA ��A	�A�bAN<A��A7LA��A_pAA@A�A��A�#A/A��A��Av`A�!A��A!�AR�A�AIRA-A�}A)_A��A[�AMjAE�A�9ArGA+kA�A�NA��A�A�Aj�A.IA��AS�A��AxA	lA�Ao A�+A�DAc�AA�A�A
�A
��A
c A
C�A
-A	�A	�2A	ĜA	w�A	V�A�~AqvAMA�DA�A��A�Ae,AJ�A��A�Ad�A �A��A{A�HA<6A�)A�oA<6A�A�A�_AW�A�A�pA4A ��A IR@��@�(@���@��@�+k@�<6@���@��)@�kQ@�"@��@�A@�O@�@�@��@�F�@�w�@��@�g�@�\@�^5@�@�l"@��N@딯@���@�C@��@�@�(�@繌@�J�@�.@�D�@�-@��5@䎊@㋬@�r�@���@��@��@�w2@ऩ@��@���@�[W@�@܃�@��#@�E9@ڶ�@�`�@�v`@���@؍�@�)_@���@�j�@Է�@�b@�,=@�,=@��W@Ӻ^@�l�@��B@ѕ�@��9@μj@��@̑�@ʚ�@Ɇ�@��@���@�O@��@Ǐ�@�k�@�N<@��@�xl@�~@�  @��9@ŕ�@�H�@�;@��@�<6@���@���@F@�@��[@�n/@�%F@�p;@��d@�rG@�
=@�_�@�9X@�u@�b�@���@�:�@���@�*0@���@��<@���@�H�@���@�[W@�P�@��s@��t@�%@��_@�4n@���@��f@���@�&�@��T@�t�@�A�@�ی@�	@���@�v`@�f�@���@�M�@�,=@���@�e,@��$@��+@�Z�@�	@���@�[W@�%@���@��/@�Ĝ@���@���@�#:@��@�ƨ@��@���@�F@�33@���@��@���@�Q@���@�RT@�8�@�n/@�t�@�6z@�Mj@�q@��@�X@��@�C-@�PH@��@�خ@��P@���@�S�@��=@�X�@��B@���@�s�@��>@���@��h@�e�@��5@��L@���@�H�@��@��@�ԕ@��3@�zx@�7L@�C@��@���@�V@���@��@���@��Z@�{J@�֡@��X@���@���@�\�@�@�@�x@��@��	@�|�@�zx@�hs@�ѷ@��$@��@�YK@�'R@��@�x�@�@@��$@�g8@�K^@��@���@�f�@�=@�9�@��@�{�@�1'@���@���@���@�Vm@�#�@���@��.@�d�@�,=@�	�@���@��n@�y�@�s�@�Vm@�?}@��[@�z@�6�@�x@��@�y�@��@�p;@��@���@���@�{J@�1�@���@�Q�@�;�@��@�j�@��@���@���@�6@�_@��=@�8�@�"�@��@���@�~(@�j@�4@���@�a@�q@���@��u@�p;@�[�@�/�@���@��!@���@�YK@��@��K@�T�@��@�+@��@��@���@�u%@�V@�2�@�u@��w@���@�P�@�*0@���@�g8@�R�@�b@�@~��@}��@|��@|,=@|@|�@{خ@{j�@{"�@z͟@zGE@y��@y^�@yf�@yzx@yIR@x��@xD�@wخ@w33@v҉@v��@v�h@v�1@u��@t�@tH@t$@s��@r�X@r�h@rz@q�@qVm@p�@pu�@o��@o��@o8@n��@m�@m%@ltT@lPH@l  @kg�@k33@k i@j�}@j{�@i��@iY�@h��@g��@g�@f��@f�+@fOv@e�o@e�#@ex�@e0�@dh�@c�w@cdZ@b��@a�@a�t@ac@aG�@`�	@`�@`*�@`M@_�@_b�@_�@^��@^u%@^E�@^
�@]�@]��@]B�@\�@\m�@[�&@Z��@Ze@Y`B@Y+@X��@X/�@W�Q@W��@Wv`@WA�@V�s@V!�@U�@Uc�@U@T�@T>B@SO@S$t@S i@R�}@R}V@Rc @R?@R�@Q�n@QDg@P�@P��@P��@Pq@PM@O�q@Oa@OW?@OJ#@N��@N��@N�A@NYK@NE�@N@M�3@M`B@Lm�@K��@K��@KJ#@J��@J��@JGE@I \@Hr�@G��@G|�@GX�@F��@F��@F��@FB[@FJ@E�9@E�h@E<6@E�@D��@D��@D��@D:�@C�P@C�@B�R@B��@Bd�@BE�@A�)@A}�@AVm@A5�@@�@@Z@?�&@?y�@?/�@>҉@>�b@>kQ@>�@=�t@=�~@=<6@<�v@<z�@<<�@<"h@;��@;!-@:�}@:��@:�@:@9�-@9s�@9J�@9!�@8ی@8I�@8'R@7�]@7�*@7P�@7�@6�@6��@6Ov@68�@6$�@6e@5�@5�@5�-@5��@5�~@5N<@5�@4�j@4r�@4j@4PH@46@3�r@3��@3{J@3e�@39�@3�@2ff@1�>@1s�@1�@0�@0h�@0b@/�g@/t�@/J#@/4�@.�"@.��@.W�@-��@-�h@-Q�@-�@,֡@,�$@,r�@,�@+��@+j�@+�@*�R@*�A@*Z�@*+k@*	@)�@)�@)�-@)k�@)/@(��@(S�@(�@'�&@'��@'��@'��@'�:@'g�@'K�@'@&�@&�@&��@&;�@%�@%j@$�f@$�4@$[�@$-�@$  @#�F@#{J@#6z@#�@"�@"p;@"a|@"@!�#@!�h@!O�@!@!	l@ �`@ �p@ ��@ l"@ @�a@��@4�@��@��@�@��@��@*0@�v@�I@bN@Ft@�@�*@g�@�c@V@�@��@�'@f�@A @�@�p@�D@PH@@  @�@��@��@��@v`@W?@,�@��@� @��@�@�9@�@F@B�@G�@5�@V@�@��@�@�/@�@l"@c�@[�@?�@�@��@��@�Q@��@��@�f@|�@F�@Y@ i@��@��@�1@kQ@�@�T@�9@��@��@�"@zx@e,@S&@q@��@�@�?@�z@��@�.@�D@bN@>B@*�@"h@�@�&@�g@�K@�0@�V@��@W?@8@!-@�@�@ں@�}@�F@~�@i�@E�@3�@&�@�@��@�9@�C@��@s�@Dg@V@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�hB	��B	� B	�B	�:B	��B	��B	�B	�tB	�B	�B	�B	�9B
�B
�B
xB
�B
�B
jB
�B
�B
�B
[B
�B
�B
#B
	B
�B
QB
B
B
B
�B
EB
�B
EB
�B
�B
_B
yB
CB
�B
!-B
*�B
UgB
�zB
��B
�RB
��B
��B
��B
�B
�B&�B1B vB,WBB�BOBBd�BpUBpB��B�eB��B��B�QB��BB��BʌB��B�RB��B�]B��B�ZB��B�hB��B�cB�B�eB��B��B��B�!B��B��Bw2Bo�BwLBj�BRoB;B*BFB
��B
�mB
ƨB
��B
��B
y$B
jB
`�B
U�B
H�B
?}B
(�B
�B
B	�B	��B	�#B	N<B	�B	�B	  B�8B�B�+B�FB�B�vB�B�B��B	pB	�B	EB	5B	 �B	)�B	)_B	7�B	\xB	m�B	�9B	ƨB	��B	�B
%B
�B
hB
�B
�B
�B
�B
�B
jB
"�B
&�B
,B
5%B
7LB
8B
9>B
<6B
?cB
?cB
@B
?�B
>�B
B�B
>B
>wB
>wB
=�B
8�B
/�B
-B
)_B
�B
B
�B	��B	��B	�zB	�-B	�B	�"B	�PB	�B
�B
�B
B
2|B
+B
B
 �B
�B
�B
�B
 OB	��B	�.B	�B
;B
�B
�B
B
B
B

rB

XB
0B

�B
tB	�"B	�wB
�B
%B
9B
-B
�B
�B
�B
�B
B
�B
�B
YB
�B
�B
�B
�B
	�B
dB
}B
�B
�B
B
�B
=B
�B
/B
xB
�B
�B
�B
�B
�B
 B
�B
7B
�B
1B
�B
�B
1B
IB
%FB
!�B
!-B
!B
 vB
 �B
#�B
%zB
%�B
%�B
%`B
$�B
#�B
"4B
�B
jB
B
B
EB
�B
JB
�B
B
 B
 �B
 �B
�B
�B
~B

�B
�B
%B
�B
B	�.B
 B
 �B	��B	�B	��B	��B	�B	��B	�*B	�XB	�B	�8B	�	B	�RB	�fB	�FB	��B	��B	��B	�nB	��B	�3B	��B	��B	��B	�[B	�B	�-B	�3B	�[B	��B	�B	�B	�/B	�)B	��B	�B	�B	�TB	�TB	�B	�B	�B	�5B	��B	�cB	��B	�QB	�,B	߾B	ݲB	�
B	�pB	�B	�lB	ȀB	ɠB	�JB	�PB	͹B	бB	өB	�_B	�QB	ںB	��B	��B	ںB	�B	��B	ؓB	��B	��B	��B	��B	ٴB	�B	�B	��B	�B	��B	چB	��B	�#B	��B	ބB	��B	�-B	�B	� B	� B	��B	��B	�B	�B	�B	��B	�NB	��B	�B	�TB	�B	�B	�nB	��B	��B	��B	�zB	�zB	�B	�8B	��B	�B	�B	�_B	��B	�DB	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�UB	�oB	�AB	�B	�hB	��B	��B	�fB	�B	�	B	�XB	��B	�B	��B	��B	�B	��B	�B	�jB	�}B
'B
�B
�B
�B
�B	�}B
�B
1B
	�B

#B

�B

rB
�B
�B
dB
�B
�B
�B
�B
�B
�B
�B
~B
�B
B
jB
�B
�B
�B
B
&B
FB
FB
,B
�B
uB
�B
�B
B
�B
{B
gB
{B
aB
�B
�B
2B
�B
�B
?B
�B
?B

B
�B
�B
�B
�B
+B
EB
�B
yB
_B
�B
yB
1B
eB
B
�B
KB
B
eB
B
=B
�B
�B
=B
�B
#B
=B
=B
WB
qB
CB
�B
IB
]B
�B
/B
�B
�B
jB
�B
�B
�B
pB
�B
�B
�B
�B
 �B
!B
!�B
"4B
"NB
"�B
#B
$@B
$�B
#�B
$�B
$�B
%�B
&B
&B
&fB
&fB
&�B
&�B
&�B
'mB
'�B
'RB
'B
&2B
&fB
&2B
%B
#nB
"�B
"�B
#�B
#nB
#nB
%,B
$�B
$�B
%FB
%FB
$�B
%�B
$�B
%�B
&�B
&2B
'B
&�B
'�B
($B
&�B
&�B
&�B
(�B
(�B
)_B
)�B
*�B
*�B
*�B
+B
+B
+B
,"B
,WB
,�B
,�B
-)B
-CB
.�B
/�B
0UB
1AB
1[B
0�B
1�B
0�B
1B
1[B
1�B
2�B
2�B
2|B
2�B
1�B
2�B
2�B
2|B
2aB
1�B
2|B
2�B
2aB
2GB
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4B
4B
4B
4�B
5%B
4�B
4�B
5�B
5?B
5tB
6B
6B
6B
6�B
6�B
7B
6�B
8�B
9XB
9	B
8�B
9	B
9rB
9$B
9rB
9�B
:�B
:B
:xB
:�B
:^B
:�B
;dB
;B
<B
<6B
<�B
;�B
;B
:�B
9�B
:�B
:xB
:�B
;�B
<B
<6B
<jB
<6B
<6B
=B
=VB
=qB
=�B
>�B
>�B
@iB
A;B
A;B
B'B
A�B
BB
BuB
B'B
CGB
CGB
D3B
D�B
EB
D�B
D�B
E�B
F%B
E�B
E�B
E�B
F�B
G+B
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
I�B
I�B
J	B
J�B
J�B
J�B
K�B
K�B
KxB
LB
L�B
MB
NB
N<B
N�B
N�B
O�B
P�B
P�B
QNB
Q�B
Q�B
RTB
RB
RTB
R�B
S�B
TFB
S�B
T�B
U�B
V�B
V�B
V�B
W�B
W?B
WsB
XB
X_B
W�B
X_B
X�B
YeB
Y�B
Y�B
Y�B
YB
Z7B
Y�B
ZB
ZkB
ZQB
Z�B
Z�B
[�B
[=B
[�B
[WB
\CB
\]B
\xB
\]B
\�B
\�B
\�B
\�B
]dB
]/B
]�B
]�B
]dB
]�B
]�B
^�B
^�B
^5B
_B
^�B
_B
^�B
^�B
_;B
_�B
`B
_pB
_;B
`vB
_�B
`\B
`'B
`�B
`�B
a�B
a|B
a�B
a�B
bB
a�B
aHB
b�B
b�B
bB
b�B
cnB
c�B
c�B
c�B
c�B
d�B
dZB
e�B
fB
fB
f�B
ffB
f2B
f�B
f�B
ffB
fLB
f�B
g�B
h$B
h$B
h$B
h>B
hXB
hsB
h>B
h�B
h
B
h�B
h�B
h�B
iB
h�B
i�B
i�B
jKB
j0B
jeB
jeB
jB
k6B
j�B
j�B
kB
kkB
lWB
k�B
l=B
k�B
l�B
mB
l�B
mB
m�B
m]B
l�B
mB
nB
m�B
m�B
ncB
n}B
n�B
o5B
o B
o5B
oB
o�B
o5B
o�B
o�B
poB
p�B
p�B
qAB
q�B
q�B
q�B
rGB
raB
rGB
r�B
r�B
sMB
sB
tB
s�B
tB
tnB
uB
u�B
uZB
u�B
u�B
vFB
vzB
v�B
vzB
v�B
wB
v�B
w�B
wLB
x8B
w�B
xRB
x�B
x�B
x�B
y	B
yrB
y�B
y�B
y�B
y�B
y�B
z*B
y�B
z^B
zxB
zxB
z^B
z�B
z�B
z�B
z�B
{JB
{B
{JB
{�B
|B
|B
|B
{�B
|B
|6B
|jB
|6B
|�B
|�B
|�B
|�B
}B
}<B
}<B
}B
}qB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~(B
~]B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
B
cB
HB
HB
HB
}B
�B
�B
�B
� B
�B
�OB
�4B
�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�hB	��B	� B	�B	�:B	��B	��B	�B	�tB	�B	�B	�B	�9B
�B
�B
xB
�B
�B
jB
�B
�B
�B
[B
�B
�B
#B
	B
�B
QB
B
B
B
�B
EB
�B
EB
�B
�B
_B
yB
CB
�B
!-B
*�B
UgB
�zB
��B
�RB
��B
��B
��B
�B
�B&�B1B vB,WBB�BOBBd�BpUBpB��B�eB��B��B�QB��BB��BʌB��B�RB��B�]B��B�ZB��B�hB��B�cB�B�eB��B��B��B�!B��B��Bw2Bo�BwLBj�BRoB;B*BFB
��B
�mB
ƨB
��B
��B
y$B
jB
`�B
U�B
H�B
?}B
(�B
�B
B	�B	��B	�#B	N<B	�B	�B	  B�8B�B�+B�FB�B�vB�B�B��B	pB	�B	EB	5B	 �B	)�B	)_B	7�B	\xB	m�B	�9B	ƨB	��B	�B
%B
�B
hB
�B
�B
�B
�B
�B
jB
"�B
&�B
,B
5%B
7LB
8B
9>B
<6B
?cB
?cB
@B
?�B
>�B
B�B
>B
>wB
>wB
=�B
8�B
/�B
-B
)_B
�B
B
�B	��B	��B	�zB	�-B	�B	�"B	�PB	�B
�B
�B
B
2|B
+B
B
 �B
�B
�B
�B
 OB	��B	�.B	�B
;B
�B
�B
B
B
B

rB

XB
0B

�B
tB	�"B	�wB
�B
%B
9B
-B
�B
�B
�B
�B
B
�B
�B
YB
�B
�B
�B
�B
	�B
dB
}B
�B
�B
B
�B
=B
�B
/B
xB
�B
�B
�B
�B
�B
 B
�B
7B
�B
1B
�B
�B
1B
IB
%FB
!�B
!-B
!B
 vB
 �B
#�B
%zB
%�B
%�B
%`B
$�B
#�B
"4B
�B
jB
B
B
EB
�B
JB
�B
B
 B
 �B
 �B
�B
�B
~B

�B
�B
%B
�B
B	�.B
 B
 �B	��B	�B	��B	��B	�B	��B	�*B	�XB	�B	�8B	�	B	�RB	�fB	�FB	��B	��B	��B	�nB	��B	�3B	��B	��B	��B	�[B	�B	�-B	�3B	�[B	��B	�B	�B	�/B	�)B	��B	�B	�B	�TB	�TB	�B	�B	�B	�5B	��B	�cB	��B	�QB	�,B	߾B	ݲB	�
B	�pB	�B	�lB	ȀB	ɠB	�JB	�PB	͹B	бB	өB	�_B	�QB	ںB	��B	��B	ںB	�B	��B	ؓB	��B	��B	��B	��B	ٴB	�B	�B	��B	�B	��B	چB	��B	�#B	��B	ބB	��B	�-B	�B	� B	� B	��B	��B	�B	�B	�B	��B	�NB	��B	�B	�TB	�B	�B	�nB	��B	��B	��B	�zB	�zB	�B	�8B	��B	�B	�B	�_B	��B	�DB	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�UB	�oB	�AB	�B	�hB	��B	��B	�fB	�B	�	B	�XB	��B	�B	��B	��B	�B	��B	�B	�jB	�}B
'B
�B
�B
�B
�B	�}B
�B
1B
	�B

#B

�B

rB
�B
�B
dB
�B
�B
�B
�B
�B
�B
�B
~B
�B
B
jB
�B
�B
�B
B
&B
FB
FB
,B
�B
uB
�B
�B
B
�B
{B
gB
{B
aB
�B
�B
2B
�B
�B
?B
�B
?B

B
�B
�B
�B
�B
+B
EB
�B
yB
_B
�B
yB
1B
eB
B
�B
KB
B
eB
B
=B
�B
�B
=B
�B
#B
=B
=B
WB
qB
CB
�B
IB
]B
�B
/B
�B
�B
jB
�B
�B
�B
pB
�B
�B
�B
�B
 �B
!B
!�B
"4B
"NB
"�B
#B
$@B
$�B
#�B
$�B
$�B
%�B
&B
&B
&fB
&fB
&�B
&�B
&�B
'mB
'�B
'RB
'B
&2B
&fB
&2B
%B
#nB
"�B
"�B
#�B
#nB
#nB
%,B
$�B
$�B
%FB
%FB
$�B
%�B
$�B
%�B
&�B
&2B
'B
&�B
'�B
($B
&�B
&�B
&�B
(�B
(�B
)_B
)�B
*�B
*�B
*�B
+B
+B
+B
,"B
,WB
,�B
,�B
-)B
-CB
.�B
/�B
0UB
1AB
1[B
0�B
1�B
0�B
1B
1[B
1�B
2�B
2�B
2|B
2�B
1�B
2�B
2�B
2|B
2aB
1�B
2|B
2�B
2aB
2GB
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4B
4B
4B
4�B
5%B
4�B
4�B
5�B
5?B
5tB
6B
6B
6B
6�B
6�B
7B
6�B
8�B
9XB
9	B
8�B
9	B
9rB
9$B
9rB
9�B
:�B
:B
:xB
:�B
:^B
:�B
;dB
;B
<B
<6B
<�B
;�B
;B
:�B
9�B
:�B
:xB
:�B
;�B
<B
<6B
<jB
<6B
<6B
=B
=VB
=qB
=�B
>�B
>�B
@iB
A;B
A;B
B'B
A�B
BB
BuB
B'B
CGB
CGB
D3B
D�B
EB
D�B
D�B
E�B
F%B
E�B
E�B
E�B
F�B
G+B
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
I�B
I�B
J	B
J�B
J�B
J�B
K�B
K�B
KxB
LB
L�B
MB
NB
N<B
N�B
N�B
O�B
P�B
P�B
QNB
Q�B
Q�B
RTB
RB
RTB
R�B
S�B
TFB
S�B
T�B
U�B
V�B
V�B
V�B
W�B
W?B
WsB
XB
X_B
W�B
X_B
X�B
YeB
Y�B
Y�B
Y�B
YB
Z7B
Y�B
ZB
ZkB
ZQB
Z�B
Z�B
[�B
[=B
[�B
[WB
\CB
\]B
\xB
\]B
\�B
\�B
\�B
\�B
]dB
]/B
]�B
]�B
]dB
]�B
]�B
^�B
^�B
^5B
_B
^�B
_B
^�B
^�B
_;B
_�B
`B
_pB
_;B
`vB
_�B
`\B
`'B
`�B
`�B
a�B
a|B
a�B
a�B
bB
a�B
aHB
b�B
b�B
bB
b�B
cnB
c�B
c�B
c�B
c�B
d�B
dZB
e�B
fB
fB
f�B
ffB
f2B
f�B
f�B
ffB
fLB
f�B
g�B
h$B
h$B
h$B
h>B
hXB
hsB
h>B
h�B
h
B
h�B
h�B
h�B
iB
h�B
i�B
i�B
jKB
j0B
jeB
jeB
jB
k6B
j�B
j�B
kB
kkB
lWB
k�B
l=B
k�B
l�B
mB
l�B
mB
m�B
m]B
l�B
mB
nB
m�B
m�B
ncB
n}B
n�B
o5B
o B
o5B
oB
o�B
o5B
o�B
o�B
poB
p�B
p�B
qAB
q�B
q�B
q�B
rGB
raB
rGB
r�B
r�B
sMB
sB
tB
s�B
tB
tnB
uB
u�B
uZB
u�B
u�B
vFB
vzB
v�B
vzB
v�B
wB
v�B
w�B
wLB
x8B
w�B
xRB
x�B
x�B
x�B
y	B
yrB
y�B
y�B
y�B
y�B
y�B
z*B
y�B
z^B
zxB
zxB
z^B
z�B
z�B
z�B
z�B
{JB
{B
{JB
{�B
|B
|B
|B
{�B
|B
|6B
|jB
|6B
|�B
|�B
|�B
|�B
}B
}<B
}<B
}B
}qB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~(B
~]B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
B
cB
HB
HB
HB
}B
�B
�B
�B
� B
�B
�OB
�4B
�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104917  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173726  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173727  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173727                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023735  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023735  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                