CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-05T00:35:24Z creation;2018-10-05T00:35:28Z conversion to V3.1;2019-12-19T07:27:18Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005003524  20200116231515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_288                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؆v4��1   @؆w�[ @4����$�dYx���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƃ3D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D��3D�  D�@ D؀ D�� D�  D�<�Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D��D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�{A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=DƀRDƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D׀RD��RD��D�=D�}DؽD��D�9�D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�@RD�}D�D��D�=D�}D�D��D�=D�}D��D��D�9�D�y�D��D��D�=D�}D�D��D�=D�}D�D� RD�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���AۑhA���A��HA�~�AӅAҾwA�M�AЕ�Aʹ9A���A�
=A˥�A���A� �A��A���A�5?A�|�A��DA��`A�+A���A�9XA���A�ZA��TA��/A�E�A��A�p�A�9XA�1A�\)A�A�ȴA���A�ƨA�t�A�%A��A�x�A��7A�n�A�|�A���A��jA��-A�~�A�C�A�ȴA��+A��!A��uA��DA�1'A�?}A��`A��uA���A��A��/A�5?A�K�A��A���A�  A��PA��A���A��RA�?}A�I�A��yA��A�Q�A�-A�XA���A�JA���A�n�A�O�A��A�\)A�
=A�C�A���A�-A���A��;A���A�K�A��A�1'A~�Aw+At�DAp�jAlȴAh�Ag��AfA�Ad��Ac/Aa�mA^�`A[\)AX��AX�`AX�`AXJAV$�ATȴAS`BAR��AQ��AP��AO��AM|�AJ��AGXAF1AE7LAC�-ABE�A@�A>�\A<9XA9�PA7�
A6�9A5��A4A�A2�/A2  A0��A/O�A.�yA.ZA-ƨA-�hA,��A*��A*$�A)`BA)
=A(�\A&9XA#O�A"{A!S�A�A��AjA�yA�
A
=A��A�mAG�A�A�/A�mAƨAt�A+AȴA�hA��A(�A33AffAAx�AM�AA|�A	ƨA	�PA	|�A	x�A	p�A�A��A
=A��A�^A1A�A�TA �A �DA ��AK�A �A;dAhs@���@���@��/@��!@�I�@�  @��m@�n�@��/@��@��@�G�@�
=@�$�@��/@�33@�E�@�1'@���@�`B@���@�  @�^5@ݲ-@�`B@��@�
=@ڗ�@�bN@�l�@�M�@Ցh@���@�+@�^5@�{@�G�@�I�@ϥ�@�@�%@̬@�A�@�  @̓u@ͺ^@�-@�7L@̓u@��@�V@�J@�O�@�1@���@���@�A�@�bN@��@Å@��7@�bN@�C�@�33@���@�`B@���@�(�@�`B@�+@�v�@�ff@���@�V@�j@�dZ@�^5@��@�?}@��j@��@�K�@���@�ff@�{@���@�?}@�Ĝ@��@��w@��@�"�@��@�v�@�E�@�$�@��#@���@�O�@�7L@��j@�bN@�9X@�1@�ƨ@�S�@���@��R@���@�ȴ@���@�$�@���@���@�`B@�/@���@��D@�bN@�A�@��@���@��@��y@��@��R@�-@��#@��T@��T@���@���@��#@���@�&�@�&�@��@��j@��@��m@�33@�
=@���@���@���@�E�@�5?@��@���@��7@�7L@��@�bN@��m@��;@���@�\)@���@�~�@�J@��T@�@�`B@�&�@��@��/@��9@�z�@�j@�I�@�  @���@��P@�;d@�@��!@�v�@�E�@�-@�$�@�@��-@�7L@��@���@��9@�b@�dZ@���@��y@���@�E�@��@�J@��@���@�`B@��@���@��@�A�@��w@�t�@�
=@���@�n�@�M�@�E�@�-@�{@��@��#@���@��@��@��u@�j@�z�@�1'@��m@�|�@�o@��H@���@��+@�v�@�=q@��T@�x�@�`B@���@�X@���@��9@���@��D@�I�@� �@��m@��w@��F@��@��F@�|�@�C�@�"�@�"�@��@�^5@�M�@���@�@���@���@�G�@��`@��D@�I�@�1'@�A�@�1@��@�1@�b@��w@�l�@�dZ@�C�@�ȴ@�^5@�@���@��^@��@�V@��`@��j@��u@�1'@��w@��P@�|�@�l�@�C�@�+@���@��!@�^5@�5?@�$�@�$�@�@��@��-@�x�@�O�@�V@�Ĝ@�Z@�  @�P@K�@~�y@~v�@}�@}@}�@}?}@}�@}�@|��@}V@|�j@|��@|9X@{�
@{��@{t�@{33@z��@z��@z^5@z-@zJ@y��@y�^@y��@yG�@x��@xĜ@x�u@x �@w�@w�P@w;d@v�R@vE�@v@u�-@uV@t�@tZ@s��@s�
@s�F@sS�@s33@r��@r^5@q��@qX@q&�@p�`@p�u@pbN@o�w@oK�@n��@n�R@n�R@nff@n{@n{@n@n@n@mp�@mO�@l�/@lj@kƨ@kt�@j�@j��@k@j�!@j-@i��@iG�@hr�@g�@g\)@f��@fff@fE�@f@e�-@e��@ep�@eV@d��@d�@dZ@dI�@c��@c�F@c"�@b�@b=q@bJ@ax�@`�`@`Q�@_�@_K�@_+@_
=@^�y@^��@^5?@]�h@\��@\��@\Z@\1@[�m@[�F@[C�@[o@Z~�@Y�^@Xr�@Xr�@W|�@Vȴ@V5?@U�T@U@U@U?}@T�@Sƨ@St�@SC�@S"�@R�\@R=q@Q�#@Q%@P�u@P �@O�;@Ol�@O
=@N�R@N5?@Mp�@L�@L�j@L�D@LI�@K�m@Kƨ@K��@KS�@J�\@I�#@IX@H��@H�u@HA�@G�@G�P@G\)@F��@Fȴ@F�R@Fff@F{@E��@E@E��@Ep�@E/@D��@D�j@D��@Dz�@D9X@Ct�@Co@B��@B��@B�\@B^5@A�#@A�7@Ahs@A7L@A%@@bN@?��@?l�@?\)@?;d@?�@>�y@>��@>5?@=�T@=�h@=`B@=O�@=O�@=O�@=O�@=O�@=O�@=O�@=/@=V@<��@<��@<��@<��@<�/@<j@;ƨ@;dZ@;33@;o@:�@:��@:�!@:�\@:n�@:^5@:M�@:=q@:�@:J@9�@9�^@9G�@8�`@8Ĝ@8�9@8�u@8bN@81'@8  @7�P@6��@6�@6�+@6E�@6@5��@5�h@5O�@5?}@5�@5�@5V@5V@4�@4�j@4�D@4I�@3��@3ƨ@3��@3dZ@3"�@2��@2~�@2^5@2-@1�#@1�7@1x�@1X@1%@0�`@0��@0��@0��@0Q�@0Q�@01'@/�;@/��@/K�@.�@.�+@.V@.{@-�T@-�-@-�@-p�@-`B@-?}@,��@,z�@,j@,I�@+��@+�
@+ƨ@+ƨ@+��@+C�@+"�@*�@*��@*��@*~�@*^5@*-@)��@)��@)&�@(��@(��@(�u@(�@(1'@(b@'�@'�;@'�@'�P@'\)@'�@&��@&�y@&ȴ@&ȴ@&ȴ@&�R@&v�@&{@%@%�-@%��@%�h@%�@%p�@%O�@%V@$��@$�j@$z�@$I�@$9X@$9X@$9X@#��@#C�@"��@"�!@"��@"~�@"-@!�@!�7@!G�@!&�@ ��@ �9@ bN@ 1'@  �@�;@l�@\)@K�@+@+@��@�y@�y@�@ȴ@��@�+@ff@@`B@�@�@�@��@�/@�@��@Z@�@�m@�@C�@33@"�@o@��@��@~�@n�@-@�@�7@G�@7L@&�@&�@�@��@�u@bN@A�@ �@b@  @�@��@�w@��@��@\)@;d@�@
=@�@�R@�+@V@5?@@��@��@p�@`B@O�@�@��@�/@�j@�j@�j@�@�D@z�@j@�@ƨ@��@��@�@�@C�@33@"�@��@�!@n�@�@��@�@�#@x�@%@�9@�u@Q�@  @��@�@l�@;d@�@��@�y@��@V@E�@5?@@@�T@�-@p�@`B@?}@V@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���AۑhA���A��HA�~�AӅAҾwA�M�AЕ�Aʹ9A���A�
=A˥�A���A� �A��A���A�5?A�|�A��DA��`A�+A���A�9XA���A�ZA��TA��/A�E�A��A�p�A�9XA�1A�\)A�A�ȴA���A�ƨA�t�A�%A��A�x�A��7A�n�A�|�A���A��jA��-A�~�A�C�A�ȴA��+A��!A��uA��DA�1'A�?}A��`A��uA���A��A��/A�5?A�K�A��A���A�  A��PA��A���A��RA�?}A�I�A��yA��A�Q�A�-A�XA���A�JA���A�n�A�O�A��A�\)A�
=A�C�A���A�-A���A��;A���A�K�A��A�1'A~�Aw+At�DAp�jAlȴAh�Ag��AfA�Ad��Ac/Aa�mA^�`A[\)AX��AX�`AX�`AXJAV$�ATȴAS`BAR��AQ��AP��AO��AM|�AJ��AGXAF1AE7LAC�-ABE�A@�A>�\A<9XA9�PA7�
A6�9A5��A4A�A2�/A2  A0��A/O�A.�yA.ZA-ƨA-�hA,��A*��A*$�A)`BA)
=A(�\A&9XA#O�A"{A!S�A�A��AjA�yA�
A
=A��A�mAG�A�A�/A�mAƨAt�A+AȴA�hA��A(�A33AffAAx�AM�AA|�A	ƨA	�PA	|�A	x�A	p�A�A��A
=A��A�^A1A�A�TA �A �DA ��AK�A �A;dAhs@���@���@��/@��!@�I�@�  @��m@�n�@��/@��@��@�G�@�
=@�$�@��/@�33@�E�@�1'@���@�`B@���@�  @�^5@ݲ-@�`B@��@�
=@ڗ�@�bN@�l�@�M�@Ցh@���@�+@�^5@�{@�G�@�I�@ϥ�@�@�%@̬@�A�@�  @̓u@ͺ^@�-@�7L@̓u@��@�V@�J@�O�@�1@���@���@�A�@�bN@��@Å@��7@�bN@�C�@�33@���@�`B@���@�(�@�`B@�+@�v�@�ff@���@�V@�j@�dZ@�^5@��@�?}@��j@��@�K�@���@�ff@�{@���@�?}@�Ĝ@��@��w@��@�"�@��@�v�@�E�@�$�@��#@���@�O�@�7L@��j@�bN@�9X@�1@�ƨ@�S�@���@��R@���@�ȴ@���@�$�@���@���@�`B@�/@���@��D@�bN@�A�@��@���@��@��y@��@��R@�-@��#@��T@��T@���@���@��#@���@�&�@�&�@��@��j@��@��m@�33@�
=@���@���@���@�E�@�5?@��@���@��7@�7L@��@�bN@��m@��;@���@�\)@���@�~�@�J@��T@�@�`B@�&�@��@��/@��9@�z�@�j@�I�@�  @���@��P@�;d@�@��!@�v�@�E�@�-@�$�@�@��-@�7L@��@���@��9@�b@�dZ@���@��y@���@�E�@��@�J@��@���@�`B@��@���@��@�A�@��w@�t�@�
=@���@�n�@�M�@�E�@�-@�{@��@��#@���@��@��@��u@�j@�z�@�1'@��m@�|�@�o@��H@���@��+@�v�@�=q@��T@�x�@�`B@���@�X@���@��9@���@��D@�I�@� �@��m@��w@��F@��@��F@�|�@�C�@�"�@�"�@��@�^5@�M�@���@�@���@���@�G�@��`@��D@�I�@�1'@�A�@�1@��@�1@�b@��w@�l�@�dZ@�C�@�ȴ@�^5@�@���@��^@��@�V@��`@��j@��u@�1'@��w@��P@�|�@�l�@�C�@�+@���@��!@�^5@�5?@�$�@�$�@�@��@��-@�x�@�O�@�V@�Ĝ@�Z@�  @�P@K�@~�y@~v�@}�@}@}�@}?}@}�@}�@|��@}V@|�j@|��@|9X@{�
@{��@{t�@{33@z��@z��@z^5@z-@zJ@y��@y�^@y��@yG�@x��@xĜ@x�u@x �@w�@w�P@w;d@v�R@vE�@v@u�-@uV@t�@tZ@s��@s�
@s�F@sS�@s33@r��@r^5@q��@qX@q&�@p�`@p�u@pbN@o�w@oK�@n��@n�R@n�R@nff@n{@n{@n@n@n@mp�@mO�@l�/@lj@kƨ@kt�@j�@j��@k@j�!@j-@i��@iG�@hr�@g�@g\)@f��@fff@fE�@f@e�-@e��@ep�@eV@d��@d�@dZ@dI�@c��@c�F@c"�@b�@b=q@bJ@ax�@`�`@`Q�@_�@_K�@_+@_
=@^�y@^��@^5?@]�h@\��@\��@\Z@\1@[�m@[�F@[C�@[o@Z~�@Y�^@Xr�@Xr�@W|�@Vȴ@V5?@U�T@U@U@U?}@T�@Sƨ@St�@SC�@S"�@R�\@R=q@Q�#@Q%@P�u@P �@O�;@Ol�@O
=@N�R@N5?@Mp�@L�@L�j@L�D@LI�@K�m@Kƨ@K��@KS�@J�\@I�#@IX@H��@H�u@HA�@G�@G�P@G\)@F��@Fȴ@F�R@Fff@F{@E��@E@E��@Ep�@E/@D��@D�j@D��@Dz�@D9X@Ct�@Co@B��@B��@B�\@B^5@A�#@A�7@Ahs@A7L@A%@@bN@?��@?l�@?\)@?;d@?�@>�y@>��@>5?@=�T@=�h@=`B@=O�@=O�@=O�@=O�@=O�@=O�@=O�@=/@=V@<��@<��@<��@<��@<�/@<j@;ƨ@;dZ@;33@;o@:�@:��@:�!@:�\@:n�@:^5@:M�@:=q@:�@:J@9�@9�^@9G�@8�`@8Ĝ@8�9@8�u@8bN@81'@8  @7�P@6��@6�@6�+@6E�@6@5��@5�h@5O�@5?}@5�@5�@5V@5V@4�@4�j@4�D@4I�@3��@3ƨ@3��@3dZ@3"�@2��@2~�@2^5@2-@1�#@1�7@1x�@1X@1%@0�`@0��@0��@0��@0Q�@0Q�@01'@/�;@/��@/K�@.�@.�+@.V@.{@-�T@-�-@-�@-p�@-`B@-?}@,��@,z�@,j@,I�@+��@+�
@+ƨ@+ƨ@+��@+C�@+"�@*�@*��@*��@*~�@*^5@*-@)��@)��@)&�@(��@(��@(�u@(�@(1'@(b@'�@'�;@'�@'�P@'\)@'�@&��@&�y@&ȴ@&ȴ@&ȴ@&�R@&v�@&{@%@%�-@%��@%�h@%�@%p�@%O�@%V@$��@$�j@$z�@$I�@$9X@$9X@$9X@#��@#C�@"��@"�!@"��@"~�@"-@!�@!�7@!G�@!&�@ ��@ �9@ bN@ 1'@  �@�;@l�@\)@K�@+@+@��@�y@�y@�@ȴ@��@�+@ff@@`B@�@�@�@��@�/@�@��@Z@�@�m@�@C�@33@"�@o@��@��@~�@n�@-@�@�7@G�@7L@&�@&�@�@��@�u@bN@A�@ �@b@  @�@��@�w@��@��@\)@;d@�@
=@�@�R@�+@V@5?@@��@��@p�@`B@O�@�@��@�/@�j@�j@�j@�@�D@z�@j@�@ƨ@��@��@�@�@C�@33@"�@��@�!@n�@�@��@�@�#@x�@%@�9@�u@Q�@  @��@�@l�@;d@�@��@�y@��@V@E�@5?@@@�T@�-@p�@`B@?}@V@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
�B
�B
�!B
�-B
�3B
�3B
�3B
�9B
�9B
�3B
�-B
�B
��B
�=B
�JB
�hB
�9B
�`B
�/BB%B#�BT�B~�Bm�B�7B�9B��B��B�^B��B�/B�;B�sB�B��B�B�B1BVB�B �B,B-B'�B�B49B7LB9XB<jB<jB)�B&�BbB1B	7B��B��B�B�BB��B�fB��BĜB�dB�'B�B��B�?B�-B��B��B��B��B��B��B��B�JBjBe`Bk�BQ�B:^B�BoB�B+B
��B
�B
�B
�`B
�NB
�ZB
�BB
��B
�}B
��B
��B
��B
w�B
cTB
gmB
o�B
ffB
T�B
8RB	��B	��B	��B	�B	jB	S�B	jB	`BB	Q�B	C�B	;dB	�B	  B��B	�B	bB	B�fB�B��B	%B	B��B��B�BŢB�^B��B��BƨB��B�wB�B��B��B��B�!B�B��B��B��B�B��BÖB��B��B��B�RB�BǮBBB�LB��Bu�B�VB�PB�+B�+B�PB� B�B�DB�VB�PB�VB�uB��B�+Bz�B��B��B��B�VB��B��B��B��B��B��B��B��B�hB�\B��B�B�!B�3B�XB�B�BÖB�wB��B�PBv�Bz�B�=B�hB��B��B��B��B�BgmB|�By�Bs�B�B�Bv�Bv�Bk�Bu�Br�BgmBl�BiyBdZBm�BjBgmBx�Bx�Bs�Bp�B~�B�B}�B~�B�Bx�B�B�%B�7B�JB�%B�VB�oB�VB�\B�bB�VB��B��B��B��B�-B�dBB�dB�qB�qB�LBBŢB�}B��B��B�wBǮB��BŢB�jB�}B�wB��BÖB��B��B��B�)B��B��B�B�B��B�B�B�#B�NB�NB�fB�fB�sB�B�B��B��B��B��B��B	B	+B	1B	
=B	PB	hB	{B	{B	�B	�B	�B	�B	�B	#�B	$�B	&�B	'�B	+B	2-B	33B	5?B	6FB	5?B	:^B	;dB	=qB	A�B	@�B	D�B	H�B	H�B	H�B	F�B	J�B	Q�B	S�B	R�B	P�B	VB	^5B	_;B	_;B	_;B	aHB	`BB	cTB	k�B	jB	hsB	jB	iyB	jB	s�B	u�B	u�B	u�B	v�B	{�B	{�B	~�B	�B	�B	�B	�B	�%B	�DB	�PB	�VB	�PB	�hB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�?B	�FB	�?B	�?B	�?B	�XB	�XB	�LB	�RB	�RB	�wB	ÖB	B	B	ƨB	ǮB	ǮB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�)B	�#B	�)B	�)B	�B	�B	�B	�/B	�NB	�HB	�;B	�BB	�;B	�TB	�NB	�ZB	�`B	�`B	�ZB	�NB	�fB	�sB	�fB	�`B	�sB	�sB	�yB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	��B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
DB
DB
JB
PB
PB
PB
VB
VB
\B
\B
VB
hB
oB
oB
hB
uB
uB
{B
{B
{B
{B
�B
{B
{B
�B
�B
{B
�B
{B
{B
uB
{B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
#�B
 �B
 �B
 �B
 �B
�B
!�B
!�B
"�B
$�B
&�B
%�B
%�B
'�B
'�B
'�B
(�B
)�B
(�B
+B
)�B
+B
(�B
+B
)�B
+B
+B
+B
,B
,B
.B
/B
/B
/B
/B
.B
.B
/B
1'B
1'B
2-B
2-B
2-B
1'B
1'B
0!B
0!B
.B
49B
1'B
1'B
2-B
49B
6FB
7LB
6FB
5?B
5?B
7LB
8RB
9XB
8RB
9XB
:^B
9XB
:^B
<jB
<jB
<jB
<jB
=qB
<jB
<jB
=qB
?}B
?}B
?}B
?}B
@�B
@�B
?}B
=qB
>wB
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
C�B
D�B
E�B
F�B
F�B
E�B
F�B
F�B
F�B
G�B
G�B
E�B
D�B
F�B
G�B
H�B
H�B
H�B
G�B
H�B
I�B
H�B
H�B
G�B
G�B
J�B
K�B
K�B
K�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
N�B
M�B
M�B
M�B
L�B
J�B
J�B
K�B
L�B
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
O�B
O�B
N�B
N�B
O�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
O�B
O�B
R�B
Q�B
R�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
S�B
S�B
VB
VB
VB
VB
VB
XB
W
B
W
B
XB
XB
YB
XB
W
B
YB
XB
XB
XB
XB
XB
YB
ZB
YB
ZB
[#B
[#B
\)B
\)B
[#B
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
]/B
^5B
_;B
`BB
`BB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
aHB
aHB
`BB
_;B
aHB
bNB
cTB
bNB
bNB
bNB
bNB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
bNB
aHB
_;B
bNB
dZB
dZB
cTB
cTB
cTB
cTB
dZB
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
gmB
gmB
ffB
e`B
e`B
hsB
iyB
iyB
iyB
hsB
hsB
iyB
hsB
hsB
iyB
hsB
jB
k�B
k�B
k�B
jB
jB
k�B
k�B
jB
jB
jB
k�B
l�B
m�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
o�B
o�B
q�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
�B
�!B
�-B
�3B
�3B
�MB
�9B
�TB
�MB
�GB
��B
��B
�pB
�}B
�SB
�B
�8B
��B_B)B%�BW
B�4Bq�B�6B��B�nB��B��B��BބB��B�yB��B�BB�B
XB�B�B!�B,�B-�B)�B"B6�B9�B;�B?}B?}B-wB)�BFB
�BDB�B��B�B��B�?B��BϫB��B��B��B��B��B��B�MB��B�B��B�hB��B��B��B��Bo�Bg�BmCBU2B>(B!�B�BYB	RB
�^B
�B
��B
�B
�:B
��B
��B
��B
��B
��B
��B
�WB
{�B
f�B
iyB
poB
g�B
VmB
:�B
{B	��B	�_B	��B	o5B	X+B	k�B	b4B	S�B	E�B	=�B	~B	B�}B	�B	�B	[B�B�hB��B	+B	uB�qB��B�qB�lB�]BЗB�FB��BB��B�/B�B�B�B��B��B��B��B�mB��B��B��B�[B�UB�UB��B��B�fBðB�GB��B��ByXB��B��B�B��B�pB�'B��B�JB�B�VB�(B�B�B��B}qB��B�VB�kB�B��B��B��B��B��B�zB�qB�&B�aB�hB�
B�CB�UB��B�*B��B��B�B�.B�;B��By�B|6B��B��B�NB�WB�nB�:B��BjB~BB{dBu?B�{B��BxBw�Bl�BvzBs�Bh�BmCBj�Be�BncBlBh�ByrByrBt�Bq�B}B��BB�B��BzDB��B�B��B�B�EB��B��B�(B�.B�4B��B�$B�B� B�B��B��BªB�B��B�B�RB��B�YB��B��B�uB�cB��B�)B�tB��B�iB�cB� BāB�.B�PB�<B��B�4BՁB�7B�yBյBڠB�	B��B�B��B��B��B�*B�'B�B�B�$B�rB�qB��B	gB	zB	�B	
�B	�B	�B	�B	�B	�B	B	�B	B	!B	#�B	%B	'RB	(sB	+kB	2-B	3MB	5?B	6�B	5�B	:�B	;�B	=�B	A�B	@�B	D�B	H�B	IB	IB	GB	K)B	RB	T,B	S&B	QhB	V9B	^OB	_;B	_pB	_pB	a|B	`�B	c�B	k�B	j�B	h�B	j�B	jB	kB	s�B	u�B	u�B	u�B	v�B	|B	|6B	HB	�AB	�oB	��B	�SB	�tB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�&B	�2B	�B	�QB	�]B	�}B	�[B	�hB	�tB	�`B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�6B	�"B	�VB	�B	�HB	�,B	�2B	�+B	�=B	�WB	�CB	�WB	�CB	�CB	چB	�B	چB	�dB	�B	�B	ߊB	��B	ߤB	�nB	�B	�B	�zB	�B	��B	�B	�fB	�XB	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�KB	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	�B	�B	��B	�B	��B	��B	�B	��B	�B	�	B	�>B	�JB	�B
 B
 B
 B
 B	�HB	�.B	�.B
 B
-B
B
3B
SB
MB
MB
gB
gB
gB
{B
�B
YB
EB
_B
fB
	lB
^B
^B
~B
jB
PB
�B
pB
pB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
 B
�B
#�B
!B
 �B
!B
!B
 'B
!�B
!�B
# B
$�B
'B
%�B
&B
(
B
(
B
($B
)B
*0B
)B
+6B
*KB
+B
)DB
+6B
*0B
+B
+6B
+6B
,WB
,WB
.IB
/OB
/OB
/OB
/5B
.IB
.cB
/iB
1AB
1AB
2aB
2GB
2aB
1[B
1AB
0�B
0�B
.�B
49B
1�B
1vB
2aB
4TB
6`B
7�B
6zB
5�B
5�B
7fB
8�B
9rB
8�B
9�B
:�B
9�B
:�B
<�B
<�B
<�B
<�B
=�B
<�B
<�B
=�B
?�B
?�B
?�B
?�B
@�B
@�B
?�B
=�B
>�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
C�B
D�B
E�B
F�B
F�B
E�B
F�B
F�B
F�B
G�B
G�B
E�B
EB
F�B
G�B
H�B
H�B
H�B
G�B
H�B
I�B
H�B
H�B
HB
G�B
J�B
K�B
K�B
K�B
J�B
J�B
KB
K�B
MB
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
NB
NB
N�B
M�B
M�B
M�B
MB
KB
KB
K�B
MB
M�B
NB
OB
N�B
N�B
O�B
O�B
O�B
O�B
PB
PB
O�B
OB
O(B
PB
Q�B
RB
RB
RB
QB
QB
P.B
P.B
SB
RB
SB
RB
SB
SB
TB
UB
UB
UB
T�B
UB
U2B
TB
TB
TB
T,B
UB
UB
UB
TB
T,B
VB
V9B
VB
VB
V9B
X+B
W$B
W?B
XEB
X+B
YB
X+B
W?B
Y1B
XEB
X+B
X+B
X+B
X_B
Y1B
ZQB
Y1B
Z7B
[=B
[=B
\)B
\]B
[=B
ZkB
[WB
\]B
\CB
\]B
]IB
]/B
]IB
\]B
\]B
]IB
]IB
]IB
^OB
^OB
^jB
^jB
]IB
]IB
]~B
^OB
_pB
`BB
`vB
_pB
`vB
`\B
`\B
`\B
`\B
`\B
`\B
abB
aHB
a|B
bhB
abB
a|B
`\B
_pB
abB
bNB
cnB
bhB
bNB
bhB
bhB
a|B
bhB
bhB
b�B
bhB
cTB
cnB
b�B
a|B
_�B
bhB
dZB
dZB
cnB
cnB
cnB
c�B
d�B
ezB
d�B
d�B
d�B
e�B
ezB
e�B
e�B
g�B
gmB
g�B
gmB
g�B
gmB
h�B
g�B
gmB
g�B
g�B
f�B
e�B
e�B
h�B
i�B
iyB
i�B
h�B
h�B
iyB
h�B
h�B
i�B
h�B
j�B
k�B
k�B
k�B
j�B
j�B
k�B
k�B
j�B
j�B
j�B
k�B
l�B
m�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
o�B
o�B
q�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
xB
w�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810090032402018100900324020181009003240201810090200252018100902002520181009020025201810100024412018101000244120181010002441  JA  ARFMdecpA19c                                                                20181005093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181005003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181005003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181005003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181005003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181005003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181005003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181005003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181005003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181005003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20181005005742                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181005153739  CV  JULD            G�O�G�O�F�3�                JM  ARCAJMQC2.0                                                                 20181008153240  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181008153240  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181008170025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181009152441  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231515                      G�O�G�O�G�O�                