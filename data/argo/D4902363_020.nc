CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-25T21:35:49Z creation;2016-07-25T21:35:54Z conversion to V3.1;2019-12-19T08:34:41Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20160725213549  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_020                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׾3(d  1   @׾3���@<'�kP���dp����>1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D3D�� D�  D�@ DÀ D�� D���D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ Dʼ�D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�3D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @'
=@z=q@��A (�A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�{A�G�B=qB��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D��Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*��D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D2 �D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=DRD½D��D�=D�}DýD���D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}Dʹ�D��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD� RD�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D׀RD׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�0R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�7LA�5?A�;dA�9XA�7LA�7LA�7LA�7LA�7LA�33A�33A��A�bA���A��A�{A�jA��A�bA�p�A���A���A��A�VA�jA��A�K�A�\)A�z�A��wA�/A�&�A���A���A���A��jA�=qA��A�~�A���A�VA�?}A�l�A�A��A�ZA��9A��wA�A�A�p�A�I�A���A��A��PA��TA�$�A��A���A���A��PA�^5A���A���A��A�=qA��A���A�M�A��;A��A�`BA���A��jA�G�A��yA��uA��A�
=A~-A}�A{�Az�9AzJAy��Ax��Aw�mAw%Av�uAv�Au`BAt1'As&�Ar��ArJAp��Ao|�An�HAnZAml�Al��Ak�wAi�7Agp�Ae\)Ad�yAdr�AchsAa�PA_�-A^��A]S�A[��AZĜAXjAW�mAW�FAW7LAV�!AV�AU��AT�/AS�PARM�AR1AQ��AO��AOS�AN��AMXAK�mAJ��AH��AHE�AG�hAGS�AF�`AE��AE
=ADjAC�hACS�ABv�A@�9A?�
A?\)A?�A>��A>E�A=7LA<ĜA<��A;��A;O�A:M�A9�hA9dZA8�jA8~�A8I�A7K�A6�jA6A�A5|�A4jA2jA1�A0-A/�A.I�A,��A+��A+l�A+C�A*�HA)��A(E�A&bA%x�A$M�A#C�A"�`A"�jA"bNA"JA!A!;dA ��A`BA�
A;dA=qA�#Al�Az�AdZAĜAVA�A/A�AI�A  A�A�mAXA�AZA&�AĜA�A��A;dA�+A�wA
~�A	�A	�wA	dZAl�A��A�yA�A��AXA��A�A1'A��A ȴA 9X@�|�@�
=@��@��H@��R@��\@�ff@�-@�/@��@��h@��m@�@�hs@��
@�\@��@�Z@�dZ@��#@�V@�bN@�b@�!@�/@��`@��@�ƨ@�t�@��H@�ȴ@�\@�h@�;d@�@�1@�
=@�{@ٙ�@�t�@ԋD@�dZ@Ѳ-@��`@Гu@�1'@��
@��@�V@��
@�C�@�^5@���@ȣ�@� �@�ƨ@�K�@�ff@��@�S�@�G�@���@� �@���@�ƨ@�
=@��+@�-@���@�%@�Q�@���@��m@�|�@���@��@�o@�v�@���@�I�@���@�l�@���@���@�-@��@�/@��@��`@�(�@�=q@��h@�?}@��j@��@�b@�C�@��R@��T@��^@�G�@��9@�bN@�9X@���@���@��@�|�@��\@�5?@��@��#@��-@�x�@��`@��;@��@�ff@��7@�Ĝ@�ƨ@���@�E�@�%@�bN@���@�C�@���@���@���@�/@�1@��P@�
=@��!@���@�{@���@�/@�%@� �@�+@��H@�v�@��@��h@�&�@���@���@�r�@�1'@���@�K�@��@��@�X@�7L@���@��@� �@��@�ƨ@�t�@�"�@�ȴ@�v�@�hs@�V@���@��u@�I�@�b@��w@�t�@�
=@���@�ff@�-@��@��T@�x�@�&�@��j@��@���@��D@�I�@�  @���@��m@���@�dZ@�"�@�o@�
=@���@���@�^5@�E�@�-@�{@�J@���@��@���@�p�@�?}@��@��/@���@��9@���@�z�@�I�@�  @~��@~E�@}�T@|�D@|1@|(�@|9X@|z�@|9X@{��@{33@zM�@y�#@y��@y�@xbN@xbN@x�@x��@xQ�@x��@x��@x��@z^5@z^5@z��@{"�@z��@y��@y�^@yX@z=q@{@z�@z��@zM�@y�@y��@y�7@y��@z�@z��@y�@x��@x�@xr�@xQ�@w��@v��@v{@u�T@u@u��@u�@u�@up�@u?}@t�@t��@t��@t�D@tI�@t9X@s�m@s�F@st�@r��@r=q@r-@q�#@p�`@pQ�@p �@pb@p  @p  @o�;@o�w@o��@ol�@o+@n�R@n��@n��@n��@nv�@nE�@m�-@m�h@mp�@m/@l�@l�@kƨ@k��@k�@kdZ@kC�@k33@k"�@j��@j=q@i��@i��@iX@h��@h�@hb@g��@f�@f�+@f5?@e�T@e��@e�@e?}@d�@dz�@c��@cS�@b�H@b��@b~�@b-@a�@a��@aX@a7L@`��@`�`@`�@`b@_�@_��@_|�@_;d@^��@^V@^{@]�T@]�-@]�h@]p�@]�@\�j@\Z@[��@[��@[��@[��@[�m@[ƨ@[�F@[��@[��@[t�@[dZ@[S�@["�@Z�H@Z�\@Z^5@Z�@ZJ@Y�@Y�^@YG�@Y7L@Y�@X�`@XĜ@X�@XQ�@X1'@Xb@W�w@W�w@W�w@W�@Wl�@V�R@V{@U@U�h@U`B@T�/@T(�@Sƨ@St�@SdZ@SC�@S33@So@R�H@R~�@Q��@Qx�@QX@Q%@P��@PĜ@P�@O�;@O�@O�@O�P@Ol�@O�@Nȴ@Nff@N{@M�-@L��@L�@L�D@Lj@L(�@L1@K��@K��@K�
@K�F@K@J=q@I��@I��@Ihs@I7L@I�@I%@H�9@H1'@G|�@Fȴ@F{@Ep�@D��@D�@Dz�@C�F@C��@C��@C��@C@B��@B~�@B~�@BM�@BJ@A��@A��@Ax�@AX@@��@@��@@  @?|�@?�@?�@>ȴ@>v�@>E�@>$�@=�T@=�-@=p�@=?}@<��@<�/@<��@<��@<�D@<9X@<(�@;��@;��@;�@;�@;t�@;S�@:�@:�!@:n�@:J@9��@9X@9&�@8��@8��@8�u@8�@8A�@7��@7�P@7l�@7\)@7;d@6��@6�@6ȴ@6�R@6�+@6{@5�@5O�@5V@4�@4�j@4z�@4Z@49X@49X@4�@41@3��@3ƨ@3�@333@3@2��@2��@2M�@1��@1�@1�#@1G�@0�9@0r�@/��@/l�@.�@.{@-��@-��@-�h@-�h@-�h@-�@-�@-?}@-/@-�@-V@-V@-V@,��@,�/@,��@,�D@,��@,j@,Z@,I�@,9X@+��@+��@+C�@+"�@+"�@+"�@+@*�@*�\@)��@(��@(r�@'�;@'��@';d@'�@'
=@&�R@&E�@%�T@%�-@%��@%��@%�@%�@%p�@%p�@%`B@%V@$�D@$Z@$Z@$I�@$9X@$(�@$(�@$�@$1@$1@$1@$1@#��@"�@"��@"n�@"M�@"-@!�@!��@!X@ ��@ ��@ 1'@   @�@��@�@l�@;d@+@�@�@��@�R@�+@ff@V@$�@��@?}@�@��@�@Z@9X@(�@(�@�@�@�@�@1@��@dZ@"�@��@^5@-@��@x�@x�@G�@%@%@��@��@Ĝ@��@Q�@b@�w@�@��@��@|�@l�@l�@\)@�@��@�y@��@$�@��@@@�@O�@�@�D@Z@��@t�@t�@dZ@33@�@��@�!@n�@-@�@J@��@�@�#@��@��@hs@7L@Ĝ@�u@bN@A�@ �@b@�@��@+@�y@�R@ff@5?@@��@�@p�@p�@p�@O�@�@�@��@�j@�@�@��@��@��@�D@z�@I�@1@��@C�@
��@
�!@
^5@
J@	��@	hs@	G�@	G�@	7L@	�@��@r�@Q�@1'@ �@ �@b@b@�@�@�;@��@|�@;d@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�7LA�5?A�;dA�9XA�7LA�7LA�7LA�7LA�7LA�33A�33A��A�bA���A��A�{A�jA��A�bA�p�A���A���A��A�VA�jA��A�K�A�\)A�z�A��wA�/A�&�A���A���A���A��jA�=qA��A�~�A���A�VA�?}A�l�A�A��A�ZA��9A��wA�A�A�p�A�I�A���A��A��PA��TA�$�A��A���A���A��PA�^5A���A���A��A�=qA��A���A�M�A��;A��A�`BA���A��jA�G�A��yA��uA��A�
=A~-A}�A{�Az�9AzJAy��Ax��Aw�mAw%Av�uAv�Au`BAt1'As&�Ar��ArJAp��Ao|�An�HAnZAml�Al��Ak�wAi�7Agp�Ae\)Ad�yAdr�AchsAa�PA_�-A^��A]S�A[��AZĜAXjAW�mAW�FAW7LAV�!AV�AU��AT�/AS�PARM�AR1AQ��AO��AOS�AN��AMXAK�mAJ��AH��AHE�AG�hAGS�AF�`AE��AE
=ADjAC�hACS�ABv�A@�9A?�
A?\)A?�A>��A>E�A=7LA<ĜA<��A;��A;O�A:M�A9�hA9dZA8�jA8~�A8I�A7K�A6�jA6A�A5|�A4jA2jA1�A0-A/�A.I�A,��A+��A+l�A+C�A*�HA)��A(E�A&bA%x�A$M�A#C�A"�`A"�jA"bNA"JA!A!;dA ��A`BA�
A;dA=qA�#Al�Az�AdZAĜAVA�A/A�AI�A  A�A�mAXA�AZA&�AĜA�A��A;dA�+A�wA
~�A	�A	�wA	dZAl�A��A�yA�A��AXA��A�A1'A��A ȴA 9X@�|�@�
=@��@��H@��R@��\@�ff@�-@�/@��@��h@��m@�@�hs@��
@�\@��@�Z@�dZ@��#@�V@�bN@�b@�!@�/@��`@��@�ƨ@�t�@��H@�ȴ@�\@�h@�;d@�@�1@�
=@�{@ٙ�@�t�@ԋD@�dZ@Ѳ-@��`@Гu@�1'@��
@��@�V@��
@�C�@�^5@���@ȣ�@� �@�ƨ@�K�@�ff@��@�S�@�G�@���@� �@���@�ƨ@�
=@��+@�-@���@�%@�Q�@���@��m@�|�@���@��@�o@�v�@���@�I�@���@�l�@���@���@�-@��@�/@��@��`@�(�@�=q@��h@�?}@��j@��@�b@�C�@��R@��T@��^@�G�@��9@�bN@�9X@���@���@��@�|�@��\@�5?@��@��#@��-@�x�@��`@��;@��@�ff@��7@�Ĝ@�ƨ@���@�E�@�%@�bN@���@�C�@���@���@���@�/@�1@��P@�
=@��!@���@�{@���@�/@�%@� �@�+@��H@�v�@��@��h@�&�@���@���@�r�@�1'@���@�K�@��@��@�X@�7L@���@��@� �@��@�ƨ@�t�@�"�@�ȴ@�v�@�hs@�V@���@��u@�I�@�b@��w@�t�@�
=@���@�ff@�-@��@��T@�x�@�&�@��j@��@���@��D@�I�@�  @���@��m@���@�dZ@�"�@�o@�
=@���@���@�^5@�E�@�-@�{@�J@���@��@���@�p�@�?}@��@��/@���@��9@���@�z�@�I�@�  @~��@~E�@}�T@|�D@|1@|(�@|9X@|z�@|9X@{��@{33@zM�@y�#@y��@y�@xbN@xbN@x�@x��@xQ�@x��@x��@x��@z^5@z^5@z��@{"�@z��@y��@y�^@yX@z=q@{@z�@z��@zM�@y�@y��@y�7@y��@z�@z��@y�@x��@x�@xr�@xQ�@w��@v��@v{@u�T@u@u��@u�@u�@up�@u?}@t�@t��@t��@t�D@tI�@t9X@s�m@s�F@st�@r��@r=q@r-@q�#@p�`@pQ�@p �@pb@p  @p  @o�;@o�w@o��@ol�@o+@n�R@n��@n��@n��@nv�@nE�@m�-@m�h@mp�@m/@l�@l�@kƨ@k��@k�@kdZ@kC�@k33@k"�@j��@j=q@i��@i��@iX@h��@h�@hb@g��@f�@f�+@f5?@e�T@e��@e�@e?}@d�@dz�@c��@cS�@b�H@b��@b~�@b-@a�@a��@aX@a7L@`��@`�`@`�@`b@_�@_��@_|�@_;d@^��@^V@^{@]�T@]�-@]�h@]p�@]�@\�j@\Z@[��@[��@[��@[��@[�m@[ƨ@[�F@[��@[��@[t�@[dZ@[S�@["�@Z�H@Z�\@Z^5@Z�@ZJ@Y�@Y�^@YG�@Y7L@Y�@X�`@XĜ@X�@XQ�@X1'@Xb@W�w@W�w@W�w@W�@Wl�@V�R@V{@U@U�h@U`B@T�/@T(�@Sƨ@St�@SdZ@SC�@S33@So@R�H@R~�@Q��@Qx�@QX@Q%@P��@PĜ@P�@O�;@O�@O�@O�P@Ol�@O�@Nȴ@Nff@N{@M�-@L��@L�@L�D@Lj@L(�@L1@K��@K��@K�
@K�F@K@J=q@I��@I��@Ihs@I7L@I�@I%@H�9@H1'@G|�@Fȴ@F{@Ep�@D��@D�@Dz�@C�F@C��@C��@C��@C@B��@B~�@B~�@BM�@BJ@A��@A��@Ax�@AX@@��@@��@@  @?|�@?�@?�@>ȴ@>v�@>E�@>$�@=�T@=�-@=p�@=?}@<��@<�/@<��@<��@<�D@<9X@<(�@;��@;��@;�@;�@;t�@;S�@:�@:�!@:n�@:J@9��@9X@9&�@8��@8��@8�u@8�@8A�@7��@7�P@7l�@7\)@7;d@6��@6�@6ȴ@6�R@6�+@6{@5�@5O�@5V@4�@4�j@4z�@4Z@49X@49X@4�@41@3��@3ƨ@3�@333@3@2��@2��@2M�@1��@1�@1�#@1G�@0�9@0r�@/��@/l�@.�@.{@-��@-��@-�h@-�h@-�h@-�@-�@-?}@-/@-�@-V@-V@-V@,��@,�/@,��@,�D@,��@,j@,Z@,I�@,9X@+��@+��@+C�@+"�@+"�@+"�@+@*�@*�\@)��@(��@(r�@'�;@'��@';d@'�@'
=@&�R@&E�@%�T@%�-@%��@%��@%�@%�@%p�@%p�@%`B@%V@$�D@$Z@$Z@$I�@$9X@$(�@$(�@$�@$1@$1@$1@$1@#��@"�@"��@"n�@"M�@"-@!�@!��@!X@ ��@ ��@ 1'@   @�@��@�@l�@;d@+@�@�@��@�R@�+@ff@V@$�@��@?}@�@��@�@Z@9X@(�@(�@�@�@�@�@1@��@dZ@"�@��@^5@-@��@x�@x�@G�@%@%@��@��@Ĝ@��@Q�@b@�w@�@��@��@|�@l�@l�@\)@�@��@�y@��@$�@��@@@�@O�@�@�D@Z@��@t�@t�@dZ@33@�@��@�!@n�@-@�@J@��@�@�#@��@��@hs@7L@Ĝ@�u@bN@A�@ �@b@�@��@+@�y@�R@ff@5?@@��@�@p�@p�@p�@O�@�@�@��@�j@�@�@��@��@��@�D@z�@I�@1@��@C�@
��@
�!@
^5@
J@	��@	hs@	G�@	G�@	7L@	�@��@r�@Q�@1'@ �@ �@b@b@�@�@�;@��@|�@;d@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B&�B(�B,B/BF�B��B�XB�B��By�Bp�BjB\)BO�B:^B"�BhB
=BB��B�mB�#B�FB�DBv�Bp�Bm�Bo�BjBgmBbNBM�B:^B�BhB
=B��B��B�B�B��B��B��B�Bv�Br�Bo�Bm�Bk�BhsBaHB[#B5?B"�B�B�BhB
=BB
�B
�TB
�B
��B
��B
ƨB
�qB
�'B
��B
��B
�VB
�B
~�B
{�B
u�B
n�B
gmB
cTB
\)B
S�B
K�B
B�B
@�B
;dB
49B
-B
&�B
#�B
�B
�B
VB
  B	��B	�ZB	�BB	�/B	�B	��B	�^B	�?B	�B	��B	��B	�oB	�PB	�JB	�DB	�=B	�1B	�B	�B	x�B	q�B	o�B	o�B	e`B	aHB	^5B	VB	J�B	A�B	8RB	5?B	/B	-B	)�B	$�B	"�B	"�B	�B	�B	�B	hB	JB	
=B	1B	+B	+B	B	B	B	  B��B��B��B��B��B�B�B�B�B�sB�`B�BB�B��B��B��BȴBB�}B�jB�dB�^B�3B�B��B��B��B��B��B��B��B��B�{B�oB�\B�PB�+B�B�B~�B}�Bz�Bv�Bt�Br�Bp�Bo�Bo�Bm�Bl�Bk�Bk�BgmBe`BffBbNB`BB_;B_;BZBW
BXBVBS�BR�BQ�BQ�BM�BK�BJ�BH�BH�BG�BF�BF�BF�BE�BD�BB�BB�BB�BA�BB�BA�BA�B@�B?}B=qB=qB<jB;dB6FB5?B2-B0!B.B-B+B(�B'�B%�B&�B$�B$�B#�B#�B$�B"�B"�B"�B#�B#�B$�B$�B$�B#�B"�B#�B#�B"�B$�B%�B$�B%�B$�B%�B'�B'�B(�B)�B(�B+B+B,B+B,B-B0!B2-B33B49B49B49B6FB6FB6FB7LB8RB9XB9XB8RB9XB=qB?}B@�B@�BB�BD�BE�BE�BF�BF�BH�BI�BI�BI�BI�BK�BO�BO�BP�BQ�BQ�BS�BVBW
BYBYB[#B]/B]/B]/B^5B_;B_;B_;BcTBdZBdZBdZBe`Be`BgmBk�Bm�Bo�Bs�Bu�By�B{�B~�B�B�%B�1B�=B�PB�\B�bB�oB��B��B��B��B��B��B��B��B��B��B�B�B�-B�9B�LB�XB�dB�dB�jB�wBBÖB��B��B��B��B��B�B�B�B�#B�/B�;B�HB�TB�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	+B	DB	DB	JB	JB	VB	bB	hB	{B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	&�B	'�B	(�B	)�B	+B	-B	.B	33B	6FB	9XB	;dB	<jB	=qB	=qB	>wB	?}B	B�B	F�B	G�B	H�B	I�B	K�B	L�B	M�B	N�B	R�B	S�B	T�B	XB	ZB	[#B	]/B	_;B	_;B	aHB	ffB	iyB	l�B	m�B	p�B	y�B	{�B	|�B	~�B	~�B	}�B	}�B	}�B	�B	�B	�B	�%B	�%B	�1B	�7B	�7B	�=B	�JB	�\B	�\B	�VB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�'B	�3B	�?B	�?B	�?B	�FB	�FB	�LB	�RB	�RB	�XB	�dB	�qB	�}B	�}B	��B	��B	B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
1B
	7B

=B

=B

=B
DB
DB
DB
JB
PB
PB
VB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iy11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�BB'B)yB-wB3�BPB�2B�PB��B��B|�BshBnIB`�BU�B?�B&�B�B~B�B��B�=B��B��B��BxBq�Bo Bq�BlWBi�BfLBQNB>BB�B&BJB�qB��B��B�BΊBƎB�B��Bw�Br�BpBm�Bl=Bi�BcnB_�B7LB#�BxB�B�BB�B
�B
�,B
�=B
��B
��B
�fB
��B
�3B
�4B
��B
��B
��B
�B
|�B
v�B
o�B
h>B
d&B
]/B
UgB
MB
CaB
A�B
<�B
5�B
-�B
'�B
%B
�B
1B
 B
�B	��B	�,B	�-B	��B	�_B	��B	��B	�2B	��B	�zB	�B	�@B	��B	�B	��B	�B	�B	�?B	��B	zDB	rGB	p�B	q�B	fB	b�B	`B	W�B	L~B	CGB	9rB	6B	/�B	-�B	+QB	%�B	#�B	#�B	�B	!B	�B	�B	�B	
�B	�B	KB	KB	�B	�B	�B	B	 4B��B�lB��B�?B�aB��B�cB�kB�B�B�BںB�FB�.B͟B�rB��B�B��B�jB�6B�tB��B�$B�ZB��B�#B�B�1B�
B�9B�gB��B�NB�(B�1B�SB��B� BHB|6Bw�Bu�Bs�Bq[BpUBp;Bn/Bm�BmwBl=Bh>Bf�Bg�Bc Ba|BaBaB[=BX_BYBV�BT�BT,BT�BTBN�BL�BKxBIRBI�BHfBGzBG�BG�BFtBE9BB�BB�BB�BA�BB�BA�BBBAoB@�B?HB?B?B<�B7�B6FB3MB0�B.�B./B+�B)yB(sB&�B'�B%FB%�B$@B$@B%FB# B#TB#�B%`B$�B&2B%�B%�B$�B$�B%�B$�B$B%zB&LB%`B&fB%�B'B(�B(�B)�B*�B)�B+kB+�B,�B+�B-CB.cB1vB2�B3�B4�B4�B4�B6�B6�B6�B7�B8�B9�B9�B9	B:�B>�B@ BA;BAUBC{BEBFBFBF�BGEBI7BJ#BI�BJ#BJ�BMBP}BPHBQNBRTBRoBT�BV�BW�BYeBYB[�B]�B]dB]�B^jB_pB_�B_�Bc�Bd�Bd�Bd�Be�BfBhXBl=Bn/BpUBtnBvzBzxB|�B�B��B��B��B��B��B��B��B�[B��B��B�	B��B�CB�!B�4B�&B��B��B�}B��B��B��B��B��B��B��B��B��B�B�gB�0B�.B�4B�TB�[B�SB�EB�kB�qBݘBߊB��B�B�B��B��B�B��B�B�B�$B�PB�VB	 OB	;B	[B	�B	mB	zB	^B	^B	dB	�B	�B	}B	�B	�B	�B	�B	�B	 �B	"B	#B	$B	$�B	'B	(
B	)*B	*B	+B	-CB	.cB	3�B	6�B	9�B	;�B	<�B	=�B	=�B	>�B	?�B	CB	F�B	HB	I7B	J	B	K�B	L�B	M�B	O(B	S&B	T,B	UMB	XEB	ZkB	[qB	]~B	_;B	_;B	aHB	f�B	iyB	l�B	mwB	poB	y�B	{�B	}B	.B	cB	~B	~B	}�B	��B	�9B	�mB	�tB	�YB	�fB	�RB	�7B	�#B	�JB	��B	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�XB	�IB	�;B	�oB	��B	��B	�ZB	�ZB	�tB	�FB	�`B	��B	�lB	�lB	��B	��B	��B	�}B	��B	��B	��B	��B	ðB	ðB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�4B	�:B	�&B	�,B	�2B	�mB	�?B	�EB	�KB	�QB	�QB	�QB	�WB	�WB	�]B	�jB	ߊB	�\B	�|B	�|B	�B	�B	�B	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�PB	�<B	�(B	�B	�.B
 4B
UB
GB
GB
3B
3B
MB
3B
3B
MB
SB
_B
EB
EB
KB
fB
�B
	lB

XB

=B

rB
xB
xB
xB
~B
jB
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!B
"B
"B
$&B
$�B
%�B
&B
'B
'B
'B
'B
'B
(
B
)B
*B
+6B
+6B
+B
+B
+6B
,=B
,"B
,"B
-B
-)B
-)B
-CB
-]B
.IB
.IB
.IB
/5B
/OB
0UB
0UB
0UB
1AB
1AB
1[B
1[B
2-B
2aB
2GB
2aB
3hB
3MB
3MB
3hB
3MB
3hB
4TB
4nB
5ZB
5ZB
5ZB
5ZB
6zB
6zB
6FB
6zB
6`B
6zB
6`B
7�B
7fB
7�B
8�B
8lB
8�B
8lB
9rB
9�B
9�B
:�B
:�B
:�B
;�B
<�B
=�B
=�B
>�B
>wB
>wB
>wB
>�B
>�B
>�B
>�B
?}B
?�B
?}B
?�B
?�B
?�B
?�B
?}B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
MB
MB
M�B
NB
N"B
N�B
N�B
N�B
O�B
O�B
PB
PB
PB
P�B
P�B
Q B
QB
QB
Q B
Q B
QB
Q B
QB
R B
S&B
SB
SB
S&B
SB
S�B
TB
TB
S�B
S�B
S�B
T,B
TB
T,B
TB
T,B
UB
UB
U2B
V9B
VB
V9B
V9B
VB
VB
V9B
VB
W?B
W?B
W$B
W$B
XB
X+B
XB
X+B
X+B
XB
X+B
XEB
Y1B
YKB
XEB
YKB
YKB
YB
Y1B
YKB
Z7B
ZQB
ZQB
ZQB
ZkB
[WB
[#B
[=B
[WB
\]B
\CB
\]B
\]B
\]B
]/B
]IB
]IB
]/B
]IB
]IB
]IB
]IB
]IB
]dB
^jB
^jB
^jB
^OB
^jB
_VB
_VB
_�B
`vB
`\B
`\B
`vB
`vB
a|B
abB
aHB
abB
abB
a|B
abB
bhB
bhB
bNB
bNB
bhB
bhB
bNB
bNB
bNB
b�B
bhB
cnB
c�B
c�B
d�B
ezB
ezB
f�B
f�B
f�B
g�B
gmB
g�B
g�B
g�B
g�B
hsB
g�B
h�B
h�B
hsB
hsB
h�B
hsB
hsB
h�B
h�B
h�B
i�B
i�B
i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607300035562016073000355620160730003556201806221211332018062212113320180622121133201804050403582018040504035820180405040358  JA  ARFMdecpA19c                                                                20160726063508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160725213549  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160725213550  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160725213551  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160725213552  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160725213552  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160725213553  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160725213553  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160725213554  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160725213554                      G�O�G�O�G�O�                JA  ARUP                                                                        20160725222518                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160726153241  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20160729153556  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160729153556  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190358  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031133  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                