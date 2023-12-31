CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-13T00:35:13Z creation;2016-08-13T00:35:15Z conversion to V3.1;2019-12-19T08:33:12Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20160813003513  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_026                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�³
m� 1   @�³����@;�\(��di�ڹ�Z1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @'
=@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/=qB7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C�C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D��Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D��Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzs�Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D���D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D���D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�RD�D���D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ĜA�ƨA�A�ĜA�ƨA�ƨA�AʾwAʼjAʼjAʾwAʾwAʾwA���AʸRAʧ�Aɧ�A�  AȅA��A�Q�A��`A�VA�
=A���A��!A���A�v�A��A�
=A���A�E�A��\A��A���A�=qA�1A���A�E�A��#A�^5A�ZA�ZA�(�A�ZA��\A�bNA�|�A�-A�(�A�I�A�|�A���A�I�A�{A���A�1A��A���A��jA��9A�oA�ƨA�x�A��A��A�I�A���A��uA��A��TA�t�A�  A��A���A�ƨA�JA��^A�dZA�(�A��A���A��RA�p�A��A�K�A��TA�ZA���A�O�A���A���A�"�A��A��jA�n�A�M�A�"�A�O�A�A~��A}�#A}+A|�\A{�PAz�Ay�Av$�Au%At��AtVAsK�ArffAq��Ap~�Ao`BAn�An��An1'Al�Ak��Aj��Ai��Ai7LAg�Afz�AeS�Ac
=Aal�A`r�A^5?A[�#AZ�\AZM�AYO�AX-AW��AY?}AZM�AY�TAX�AV�9AU��AUl�AT�DARv�AP��AP�uAO�AM��AM\)AM;dAL��ALv�AJ��AJ9XAI�AG��AF�AF5?AFE�AE`BAD(�ABr�AA�hAA�A@9XA?�7A>��A=�#A=�PA=7LA<n�A:��A8��A6�jA6M�A5�
A5S�A4I�A2��A1G�A0bA.JA-&�A,�!A,�A+�A+oA)`BA&��A%��A%oA$ȴA#��A#��A#dZA#A"Q�A" �A"{A"A!�#A!�hA!K�A ��A ZA {AhsAI�A�A��A  A�A^5AXA�/A%A�;A��A/AĜA9XAbAA�#A��A�!A�7AȴAC�A��A�yAI�A��A
��A
��A
��A
9XA	XA1'A�7A��An�AbNAM�A1'AbA  A�AAVA��A��A�7A �!A bN@�t�@�=q@��@�V@��7@�D@��m@�@�|�@���@�^5@�7L@��@�{@陚@��/@�t�@��@�9@�w@�+@��@�x�@�Ĝ@�r�@�Z@�-@��`@�"�@ڟ�@��T@ف@�j@�C�@�`B@��/@�Ĝ@Ԭ@�z�@��;@�(�@�$�@Ͳ-@̴9@˅@�$�@ȋD@�Q�@�(�@�+@ř�@�O�@��/@�V@���@���@���@�;d@��+@���@�O�@��@��`@���@��9@���@���@��^@�7L@��/@�A�@���@��@�j@��@���@�X@�V@���@��@���@���@�t�@�33@�+@�"�@��@���@�n�@���@��@�O�@��j@�(�@��P@�;d@�ff@��7@�G�@�&�@�%@���@���@�Z@�S�@���@�@��@�r�@��@�t�@��!@��@�p�@�?}@���@��/@��j@�j@�S�@�
=@���@�$�@�7L@�Z@��@��@���@�M�@�{@���@���@��^@�x�@�`B@�G�@�Ĝ@��@���@��
@��F@�;d@���@���@�33@��w@���@��@�dZ@�C�@���@��\@���@�bN@�ȴ@�n�@���@��@���@��h@�?}@� �@�b@��@��@���@�S�@�@��@�$�@��-@���@���@��h@�`B@��@��@���@�Ĝ@�Z@�9X@� �@�b@��m@���@���@�\)@�\)@�\)@�\)@�S�@�;d@�"�@��y@�n�@�^5@�^5@�^5@�V@�V@�V@�V@��@���@�p�@�x�@��@�G�@�V@�%@��/@�Ĝ@���@�bN@�9X@��@�  @��@��@��w@�|�@��!@���@��j@�&�@��@
=@~�@~ff@~E�@~5?@}�T@}�-@}`B@}p�@}p�@}O�@}/@|�@|�@{�m@{�F@z�@z�!@z=q@y�@y%@x��@xbN@xb@x  @w�@w��@vff@u�@up�@u�h@u@v��@w�P@w�@w�P@w;d@v��@u��@t1@s�@so@r��@r-@q��@q�#@q��@qx�@q%@pbN@o��@o��@oK�@n�y@n��@nff@m�h@m`B@m`B@l�@lZ@k�
@kƨ@j~�@i��@ix�@iG�@h��@h��@h��@h�`@h��@i%@h��@h��@h�9@h�@hQ�@hb@g��@f�y@f�+@f{@e��@e�-@e�T@e��@d�/@d�D@c��@cƨ@c��@cdZ@b�@bM�@bJ@a�@a�7@aX@a7L@a&�@a�@a%@`Ĝ@`Q�@_��@_+@^5?@]�-@]`B@]/@]V@\�/@\�@\z�@\j@\Z@\9X@[�
@[�F@[��@[t�@[C�@[o@Z�H@Z�H@Z��@Z��@Zn�@ZM�@Y��@Yx�@Y�@X�`@X��@X��@X �@W�@W�w@W�@Wl�@W
=@Vȴ@V��@V$�@U�-@U�@UO�@T�@T�@T��@TZ@T1@S�
@Sƨ@S��@S�@S@Rn�@RM�@R-@RJ@Q�#@Q��@Qx�@Q7L@PĜ@P�9@P�9@P��@PA�@O�;@OK�@N�@Nȴ@Nȴ@Nȴ@N�R@N��@M��@M?}@L�/@L�j@Lj@L(�@K�m@K�@K"�@J~�@J�@I�^@Ihs@I&�@H��@H�u@HA�@Hb@G�P@F�R@F5?@F@E�T@Ep�@EV@D�j@D��@Dz�@D�@C��@C��@C�m@Ct�@B�!@B~�@BM�@BM�@B=q@B�@AX@@��@@Ĝ@@Ĝ@@��@@Q�@?��@>��@>5?@>{@=�-@=�-@=�h@<��@<�j@<�j@<��@<j@<j@<j@<Z@<I�@<�@;�m@;��@;C�@:�@:n�@:M�@:=q@:=q@:-@:-@:�@9hs@9&�@8��@8�`@8��@8�9@8 �@7�;@7�P@7;d@6�@6ȴ@6ȴ@6V@5�@5�T@5@5V@4�@4�j@4z�@4I�@3ƨ@3�@3C�@2�@2��@2n�@2^5@2M�@2�@1�#@1��@1��@1�7@1x�@1X@1�@0�9@0�u@0�u@0 �@/�;@/�w@/�@/�@/��@/K�@/+@/+@/�@.��@.�@.ff@.E�@.5?@.@-�-@-�h@-O�@,��@,�D@,�@+dZ@+S�@+o@+@*~�@*=q@*=q@*�@)�7@)7L@(�`@(�@(1'@(b@'��@'|�@'K�@'+@'�@'
=@&�R@&v�@&ff@&ff@&ff@&E�@%�T@%��@%`B@%V@$�@$�@$�/@$�/@$�j@$�@#�
@#�
@#ƨ@#�@"�@"~�@"M�@!��@!�#@!�7@!G�@!%@ �9@ bN@  �@�@�w@��@l�@��@�@�@ȴ@�R@��@��@�T@@�-@?}@�j@�D@Z@9X@9X@(�@(�@(�@�@��@�F@dZ@��@=q@��@x�@hs@G�@7L@��@ �@�w@�P@�P@l�@;d@�@�@�+@5?@�T@�@�j@�j@��@j@(�@ƨ@�F@t�@C�@"�@o@�@�!@�\@^5@-@��@��@��@X@&�@��@�`@��@��@��@��@r�@Q�@�w@�@|�@�y@��@V@V@V@5?@@�T@��@@��@p�@V@V@��@�@�@��@��@Z@ƨ@��@��@t�@o@
�H@
��@
��@
��@
��@
��@
~�@
^5@
=q@
�@
J@	��@	��@	x�@	x�@	hs@	X@	7L@��@Ĝ@Ĝ@�9@��@r�@A�@ �@�@��@�P@|�@+@�@�@��@V@$�@5?@5?@$�@@��@p�@/@V@�@�j@Z@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ĜA�ƨA�A�ĜA�ƨA�ƨA�AʾwAʼjAʼjAʾwAʾwAʾwA���AʸRAʧ�Aɧ�A�  AȅA��A�Q�A��`A�VA�
=A���A��!A���A�v�A��A�
=A���A�E�A��\A��A���A�=qA�1A���A�E�A��#A�^5A�ZA�ZA�(�A�ZA��\A�bNA�|�A�-A�(�A�I�A�|�A���A�I�A�{A���A�1A��A���A��jA��9A�oA�ƨA�x�A��A��A�I�A���A��uA��A��TA�t�A�  A��A���A�ƨA�JA��^A�dZA�(�A��A���A��RA�p�A��A�K�A��TA�ZA���A�O�A���A���A�"�A��A��jA�n�A�M�A�"�A�O�A�A~��A}�#A}+A|�\A{�PAz�Ay�Av$�Au%At��AtVAsK�ArffAq��Ap~�Ao`BAn�An��An1'Al�Ak��Aj��Ai��Ai7LAg�Afz�AeS�Ac
=Aal�A`r�A^5?A[�#AZ�\AZM�AYO�AX-AW��AY?}AZM�AY�TAX�AV�9AU��AUl�AT�DARv�AP��AP�uAO�AM��AM\)AM;dAL��ALv�AJ��AJ9XAI�AG��AF�AF5?AFE�AE`BAD(�ABr�AA�hAA�A@9XA?�7A>��A=�#A=�PA=7LA<n�A:��A8��A6�jA6M�A5�
A5S�A4I�A2��A1G�A0bA.JA-&�A,�!A,�A+�A+oA)`BA&��A%��A%oA$ȴA#��A#��A#dZA#A"Q�A" �A"{A"A!�#A!�hA!K�A ��A ZA {AhsAI�A�A��A  A�A^5AXA�/A%A�;A��A/AĜA9XAbAA�#A��A�!A�7AȴAC�A��A�yAI�A��A
��A
��A
��A
9XA	XA1'A�7A��An�AbNAM�A1'AbA  A�AAVA��A��A�7A �!A bN@�t�@�=q@��@�V@��7@�D@��m@�@�|�@���@�^5@�7L@��@�{@陚@��/@�t�@��@�9@�w@�+@��@�x�@�Ĝ@�r�@�Z@�-@��`@�"�@ڟ�@��T@ف@�j@�C�@�`B@��/@�Ĝ@Ԭ@�z�@��;@�(�@�$�@Ͳ-@̴9@˅@�$�@ȋD@�Q�@�(�@�+@ř�@�O�@��/@�V@���@���@���@�;d@��+@���@�O�@��@��`@���@��9@���@���@��^@�7L@��/@�A�@���@��@�j@��@���@�X@�V@���@��@���@���@�t�@�33@�+@�"�@��@���@�n�@���@��@�O�@��j@�(�@��P@�;d@�ff@��7@�G�@�&�@�%@���@���@�Z@�S�@���@�@��@�r�@��@�t�@��!@��@�p�@�?}@���@��/@��j@�j@�S�@�
=@���@�$�@�7L@�Z@��@��@���@�M�@�{@���@���@��^@�x�@�`B@�G�@�Ĝ@��@���@��
@��F@�;d@���@���@�33@��w@���@��@�dZ@�C�@���@��\@���@�bN@�ȴ@�n�@���@��@���@��h@�?}@� �@�b@��@��@���@�S�@�@��@�$�@��-@���@���@��h@�`B@��@��@���@�Ĝ@�Z@�9X@� �@�b@��m@���@���@�\)@�\)@�\)@�\)@�S�@�;d@�"�@��y@�n�@�^5@�^5@�^5@�V@�V@�V@�V@��@���@�p�@�x�@��@�G�@�V@�%@��/@�Ĝ@���@�bN@�9X@��@�  @��@��@��w@�|�@��!@���@��j@�&�@��@
=@~�@~ff@~E�@~5?@}�T@}�-@}`B@}p�@}p�@}O�@}/@|�@|�@{�m@{�F@z�@z�!@z=q@y�@y%@x��@xbN@xb@x  @w�@w��@vff@u�@up�@u�h@u@v��@w�P@w�@w�P@w;d@v��@u��@t1@s�@so@r��@r-@q��@q�#@q��@qx�@q%@pbN@o��@o��@oK�@n�y@n��@nff@m�h@m`B@m`B@l�@lZ@k�
@kƨ@j~�@i��@ix�@iG�@h��@h��@h��@h�`@h��@i%@h��@h��@h�9@h�@hQ�@hb@g��@f�y@f�+@f{@e��@e�-@e�T@e��@d�/@d�D@c��@cƨ@c��@cdZ@b�@bM�@bJ@a�@a�7@aX@a7L@a&�@a�@a%@`Ĝ@`Q�@_��@_+@^5?@]�-@]`B@]/@]V@\�/@\�@\z�@\j@\Z@\9X@[�
@[�F@[��@[t�@[C�@[o@Z�H@Z�H@Z��@Z��@Zn�@ZM�@Y��@Yx�@Y�@X�`@X��@X��@X �@W�@W�w@W�@Wl�@W
=@Vȴ@V��@V$�@U�-@U�@UO�@T�@T�@T��@TZ@T1@S�
@Sƨ@S��@S�@S@Rn�@RM�@R-@RJ@Q�#@Q��@Qx�@Q7L@PĜ@P�9@P�9@P��@PA�@O�;@OK�@N�@Nȴ@Nȴ@Nȴ@N�R@N��@M��@M?}@L�/@L�j@Lj@L(�@K�m@K�@K"�@J~�@J�@I�^@Ihs@I&�@H��@H�u@HA�@Hb@G�P@F�R@F5?@F@E�T@Ep�@EV@D�j@D��@Dz�@D�@C��@C��@C�m@Ct�@B�!@B~�@BM�@BM�@B=q@B�@AX@@��@@Ĝ@@Ĝ@@��@@Q�@?��@>��@>5?@>{@=�-@=�-@=�h@<��@<�j@<�j@<��@<j@<j@<j@<Z@<I�@<�@;�m@;��@;C�@:�@:n�@:M�@:=q@:=q@:-@:-@:�@9hs@9&�@8��@8�`@8��@8�9@8 �@7�;@7�P@7;d@6�@6ȴ@6ȴ@6V@5�@5�T@5@5V@4�@4�j@4z�@4I�@3ƨ@3�@3C�@2�@2��@2n�@2^5@2M�@2�@1�#@1��@1��@1�7@1x�@1X@1�@0�9@0�u@0�u@0 �@/�;@/�w@/�@/�@/��@/K�@/+@/+@/�@.��@.�@.ff@.E�@.5?@.@-�-@-�h@-O�@,��@,�D@,�@+dZ@+S�@+o@+@*~�@*=q@*=q@*�@)�7@)7L@(�`@(�@(1'@(b@'��@'|�@'K�@'+@'�@'
=@&�R@&v�@&ff@&ff@&ff@&E�@%�T@%��@%`B@%V@$�@$�@$�/@$�/@$�j@$�@#�
@#�
@#ƨ@#�@"�@"~�@"M�@!��@!�#@!�7@!G�@!%@ �9@ bN@  �@�@�w@��@l�@��@�@�@ȴ@�R@��@��@�T@@�-@?}@�j@�D@Z@9X@9X@(�@(�@(�@�@��@�F@dZ@��@=q@��@x�@hs@G�@7L@��@ �@�w@�P@�P@l�@;d@�@�@�+@5?@�T@�@�j@�j@��@j@(�@ƨ@�F@t�@C�@"�@o@�@�!@�\@^5@-@��@��@��@X@&�@��@�`@��@��@��@��@r�@Q�@�w@�@|�@�y@��@V@V@V@5?@@�T@��@@��@p�@V@V@��@�@�@��@��@Z@ƨ@��@��@t�@o@
�H@
��@
��@
��@
��@
��@
~�@
^5@
=q@
�@
J@	��@	��@	x�@	x�@	hs@	X@	7L@��@Ĝ@Ĝ@�9@��@r�@A�@ �@�@��@�P@|�@+@�@�@��@V@$�@5?@5?@$�@@��@p�@/@V@�@�j@Z@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Bz�B|�B}�B� B� Bw�Bp�BbNBS�BJ�BA�B0!B�B\B�B�B�
B��B�}B��B��B��B��B��B�VB�PB�PB�=B� Bt�Be`BZBVBL�B<jB6FB.B(�B'�B#�B�B\B  B�B�ZB�#B�B��B��BŢB�}B�LB��B��B�JB�B|�Bp�B_;BO�BB�B>wB:^B5?B1'B-B#�B�B�BVBB
��B
��B
�B
�`B
�NB
�#B
�B
��B
��B
��B
ɺB
�}B
�LB
�!B
��B
��B
��B
��B
�hB
�1B
o�B
ffB
dZB
aHB
[#B
R�B
N�B
F�B
>wB
>wB
?}B
;dB
/B
"�B
�B
�B
bB
B	�B	�ZB	��B	��B	�RB	��B	�B	t�B	s�B	p�B	n�B	t�B	�DB	��B	��B	��B	��B	�\B	�JB	�%B	u�B	e`B	cTB	`BB	VB	XB	YB	YB	XB	O�B	I�B	C�B	:^B	.B	+B	+B	%�B	�B	\B	1B	B	B��B��B��B��B	B��B�B�;B��B��B��B��B��B��B��B��BƨB��B�wB�qB�dB�XB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�hB�\B�uB�uB�VB�JB�1B�%B�B{�By�Bt�Bo�Bm�Bm�Bk�BiyBiyBhsBgmBffBe`B`BB^5BZBVBR�BQ�BO�BN�BM�BM�BN�BM�BK�BI�BH�BF�BF�BF�BE�BE�BE�BD�BC�BB�BA�B=qB;dB8RB8RB7LB5?B49B33B0!B/B.B-B+B)�B)�B)�B'�B&�B&�B$�B#�B"�B!�B!�B!�B �B!�B!�B �B �B#�B"�B#�B"�B#�B#�B%�B&�B&�B&�B%�B%�B%�B$�B+B)�B(�B)�B)�B+B,B,B,B.B-B-B-B1'B49B49B6FB8RB8RB:^B:^B;dB;dB;dB;dB=qB?}BA�BB�BB�BC�BC�BH�BL�BQ�BS�BS�BT�BVBXBXBYBYBYBZBZBZB\)B[#B^5B]/B`BBaHBcTBe`Be`BhsBk�Bk�Bk�Bl�Bl�Bl�Bn�Bp�Bq�Bw�Bz�B{�B}�B�B�B�1B�7B�=B�DB�DB�DB�VB�{B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�3B�FB�^B�^B�jB�qB�wB�wB��BÖB��B��B�B�B�B�)B�)B�5B�/B�B�/B�/B�5B�HB�ZB�`B�fB�mB�yB�yB�B�B�B�B��B��B	  B	B	B	B	+B	1B		7B	JB	hB	oB	uB	{B	�B	�B	�B	�B	!�B	"�B	#�B	$�B	'�B	(�B	-B	0!B	1'B	1'B	2-B	2-B	2-B	2-B	49B	9XB	:^B	<jB	?}B	C�B	F�B	H�B	H�B	I�B	J�B	K�B	L�B	M�B	N�B	O�B	Q�B	XB	ZB	[#B	YB	VB	W
B	[#B	[#B	ZB	]/B	_;B	`BB	`BB	bNB	cTB	gmB	iyB	k�B	m�B	n�B	s�B	w�B	x�B	y�B	|�B	~�B	� B	� B	}�B	~�B	�B	�B	�B	�B	�B	�1B	�JB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�FB	�RB	�XB	�jB	B	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�)B	�5B	�5B	�;B	�;B	�HB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
PB
VB
VB
\B
\B
bB
bB
hB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
,B
-B
-B
-B
-B
-B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
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
N�B
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
Q�B
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
T�B
T�B
T�B
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
W
B
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
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
^5B
_;B
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
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
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
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�BtBtnBt�B|PB~(B�B�AB�gB�{By�BgBWsBP.BH�B6�B�B9B�RB��B��B�BB�B��B��B��B��B�sB��B��B�VB�0B�Bw2BgRB[WBXEBN�B>]B8B.�B)�B)*B%�BpBBuB��B�B��B��B��B�B��B�UB��B�6B�B��B��B.Bs3Ba�BQhBC{B?cB;B5�B2GB.�B$�B�BQB�BtB
�B
�XB
�B
�B
�:B
��B
ևB
ԕB
�}B
̳B
˒B
��B
��B
�B
��B
��B
�B
�=B
��B
�)B
p�B
f�B
e,B
b�B
\CB
S�B
PHB
G�B
>�B
>�B
@�B
<�B
0�B
$&B
�B
�B
 B
�B	�B	��B	��B	�AB	��B	�BB	��B	u�B	uB	q�B	n}B	s�B	��B	��B	�B	�B	��B	�bB	��B	��B	wfB	f�B	e,B	a�B	V�B	X_B	Y�B	ZB	ZB	P�B	K^B	E9B	;�B	.�B	+�B	,WB	'�B	�B	}B		B	YB	B�B��B�rB��B	uB�B��B�bB��B��B��BϑB��B̳BѷB�B��B�[B�B��B�B��B�+B��B��B��B��B�VB�IB�CB��B��B��B��B��B�$B�B�MB��B�B��B��B��B�\B��B�RB��B�{B}"B|BvBp!BnIBnIBl=Bi�Bi�Bh�Bh>Bg�BgBa�B`BB\BW$BS�BR�BP�BO\BNVBN�BP.BOBBL�BJ�BI7BF�BF�BF�BE�BE�BE�BE9BD�BDMBCGB>�B<�B9	B9XB8RB6�B6�B5?B1'B0!B0B-�B+�B*�B+B+kB(�B'�B'�B&B%B#�B"�B"�B"hB!HB"NB"4B!HB"NB$�B$B$ZB#�B$tB$�B&�B($B'mB'8B&LB&fB&�B'8B,"B*�B)�B*�B+B+�B,qB,qB,�B/ B-�B-�B.�B2�B5?B5�B7�B8�B8�B:�B:�B;�B;�B;�B<6B>]B@OBA�BB�BC-BDgBEBI�BN"BRTBTFBTFBU�BV�BX+BX+BYeBYKBYKBZ7BZQBZkB\�B[�B^OB]�B`�Ba�Bc�Be�Bf2BiBk�Bk�Bk�Bl�Bl�BmBoiBq'Br�Bx�B{0B|jB~�B��B��B��B��B��B��B��B��B�B��B�B�$B�KB�dB��B�$B�DB�eB�WB�=B�CB�/B�IB�iB�[B��B��B��B�xB��B��B��B��B�OBÖB��B�&B�9B�_BٚB��B��B�!B�B�BݲB�~B�jB�B��B��B�B�B�B�B��B��B��B�'B�B�B	 B	;B	AB	gB	_B	�B		lB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#B	$B	$�B	(>B	)_B	-]B	0;B	1AB	1AB	2aB	2GB	2aB	2GB	4�B	9�B	:�B	<jB	?�B	C�B	F�B	H�B	H�B	I�B	J�B	K�B	MB	M�B	N�B	PB	R B	XEB	Z�B	[�B	ZB	V9B	W$B	[�B	[�B	ZkB	]dB	_VB	`vB	`vB	b�B	cnB	g�B	i�B	k�B	m�B	n�B	s�B	xB	y	B	zDB	}"B	HB	�OB	�OB	~B	.B	�'B	�-B	�3B	�gB	��B	��B	�JB	�VB	�\B	�.B	�@B	��B	��B	��B	��B	�)B	�/B	��B	��B	��B	��B	��B	��B	��B	� B	�&B	�B	�B	��B	�B	�&B	�B	�B	�,B	��B	�$B	�DB	�DB	�=B	�UB	��B	��B	��B	�rB	��B	ªB	ðB	ÖB	ĜB	żB	��B	��B	ɺB	��B	�B	�B	�B	�NB	�&B	�,B	�,B	�B	��B	�EB	�eB	�xB	�xB	�jB	�jB	�VB	�pB	�|B	�nB	�B	�B	�zB	�zB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	�$B	��B	�B	�B	��B	�B	�B	�B	�"B	�(B	�.B	�.B
 B
 4B
 B
 B
 B
'B
'B
AB
-B
aB
MB
9B
SB
SB
SB
SB
?B
?B
_B
KB
KB
fB
�B
	�B

�B
^B
DB
^B
DB
xB
^B
�B
�B
pB
pB
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
 'B
!�B
!�B
"�B
#B
$&B
%FB
&LB
&2B
&B
'B
'B
(
B
(>B
(
B
(
B
($B
($B
(�B
(�B
)B
)B
)*B
)*B
)*B
*0B
*B
*KB
+B
+B
+B
+B
+B
+6B
+6B
,=B
-)B
-)B
-CB
-)B
-CB
.IB
/5B
/OB
0UB
0;B
0;B
0UB
1[B
1AB
2GB
2|B
3MB
3hB
4TB
4nB
4�B
5ZB
5ZB
5tB
6`B
7fB
7LB
7�B
7fB
7fB
7�B
7fB
7fB
8lB
8lB
8lB
9�B
:�B
:�B
:�B
;B
;B
<jB
<�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
J�B
K�B
K�B
K�B
K�B
MB
MB
MB
L�B
M�B
NB
M�B
OB
N�B
OB
N�B
O�B
O�B
PB
PB
Q B
Q B
P�B
Q B
Q B
Q B
Q4B
RB
SB
S@B
S@B
TB
TB
TB
S�B
S�B
TB
TB
T,B
TB
UB
UMB
U2B
VSB
VSB
W$B
W$B
W?B
W?B
W?B
XEB
Y1B
Y1B
YB
Y1B
Y1B
Y1B
YKB
YKB
ZQB
ZQB
Z�B
[=B
ZB
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
[=B
[=B
[=B
[=B
[WB
[=B
[WB
[WB
[WB
\]B
\]B
\]B
\]B
]IB
]IB
]dB
]IB
]/B
]/B
]IB
]IB
^jB
_VB
_VB
_pB
_VB
`vB
`BB
`\B
`\B
`vB
abB
abB
abB
a|B
abB
abB
bNB
bNB
bhB
bNB
bhB
b�B
b�B
c�B
dtB
d�B
dtB
d�B
e�B
e�B
ezB
e`B
e`B
e`B
ezB
e�B
f�B
f�B
f�B
f�B
f�B
g�B
gmB
g�B
g�B
g�B
g�B
h�B
hsB
hsB
h�B
h�B
h�B
i�B
i�B
i�B
iyB
i�B
i�B
j�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9%~<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608170036442016081700364420160817003644201806221212252018062212122520180622121225201804050404542018040504045420180405040454  JA  ARFMdecpA19c                                                                20160813093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160813003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160813003513  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160813003513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160813003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160813003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160813003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160813003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160813003515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160813003515                      G�O�G�O�G�O�                JA  ARUP                                                                        20160813012017                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160813153440  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160813153440  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160813153440  CV  LATITUDE        G�O�G�O�A޶F                JM  ARCAJMQC2.0                                                                 20160816153644  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160816153644  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190454  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031225  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                