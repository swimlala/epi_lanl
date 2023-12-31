CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-06-12T00:35:42Z creation;2017-06-12T00:35:45Z conversion to V3.1;2019-12-19T08:05:08Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20170612003542  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_128                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�u{r�1   @�v333 @3"�s�P�da>�6z1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]�\C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=D��D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DC �DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD���D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�9�D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�3�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�O�A�(�A��A��A��A�v�Aש�A��/A�{A��A�$�A��A��A�&�A�1'A�=qA�A�A�Q�A�XA�&�Aח�A�1AԍPAӗ�AҋDA��`Aћ�A���A�;dA�  A�"�A�ƨA�ƨA�O�A�v�A�-A�A��AʃA���A��#A�/A�9XAƸRA�C�A��A�x�A�{A�?}A�1AøRA�A�A��wA�`BA�v�A�dZA��A�=qA�=qA��;A�bNA�A���A�Q�A�C�A�bA���A�7LA��#A�K�A���A��jA�C�A��DA�^5A�v�A��
A�I�A��
A�bNA�7LA�"�A�C�A�G�A��A�VA�ffA��A��FA���A�p�A��A�`BA�ffA�-A�A�A���A�\)A�+A���A�1'A�I�A���A�O�A�z�A�  A��A���A�1A�9XA�A��+A�%A�^5A��FA��FA�E�A���A���A��A��`A��RA��Ap�A}�
AzAx(�AvA�As
=Am�Ai�^Af�Ad=qAa��A`jA^�yA\ĜA\�A\bA\9XA[�mAZ�AY�
AY"�AW33AU�AS�mAR�AQ��AP�9AN�uAMx�AL�AK��AJ=qAF�`AD~�AC\)AB�RAB{AA��AA;dA@1A=ƨA;��A:v�A9��A9
=A8ȴA7�A7�TA7��A6-A5C�A2�9A1�A1;dA0�A,ĜA,A+p�A*�uA'��A&�A&n�A&-A%�TA%
=A"��A"��A"�uA"�A �jA {A   Al�AbAI�A7LA�wA�A$�A�A�AbA�A��A~�A�wAl�AZAVAbAjA-A�A�wAoA	��A�A^5A�FA
=A ĜA �uA M�@�S�@�@�=q@���@��@�7L@��!@��@��
@�o@�J@���@�%@�r�@�b@�@�@�R@�-@��/@�bN@@�Z@�;d@�@�\@�n�@�V@�J@�O�@��/@�9X@�;d@�?}@�A�@�ȴ@��@�{@��@�@�%@�1'@�"�@�{@�9X@���@��/@�33@թ�@�n�@Ѓ@ύP@�33@�n�@�-@��@̣�@�1'@�  @��
@ˍP@�;d@�o@ʰ!@��T@�hs@�%@ȣ�@��m@�dZ@�=q@�hs@�%@�j@���@Å@��H@°!@�v�@��^@¸R@�-@�S�@���@���@��/@�Ĝ@�ȴ@�z�@���@���@��@���@�j@�1@���@�@��@���@�ff@�=q@��!@�@��@�%@�Q�@��@��P@�
=@�ȴ@��\@�@�p�@�&�@��@���@���@��H@�n�@���@�V@�t�@�n�@�@���@��@��9@�z�@�Q�@�A�@�1'@� �@��@���@�33@���@��@���@�^5@��@�X@�/@�V@��9@��@�r�@�I�@�(�@���@���@�|�@�S�@�@�{@���@���@��@�x�@�hs@�hs@�hs@�%@��9@��@��@�ƨ@��w@���@�o@��\@��@��-@�x�@�?}@��9@�A�@��@�1@��m@���@��P@�l�@�K�@�o@��H@���@���@��+@�v�@�-@���@���@��7@�p�@�O�@�&�@��`@�j@��@��
@�t�@���@�~�@�^5@��@��@��@��T@��T@��#@���@���@��h@�?}@�V@���@���@�r�@�9X@�1@��F@�l�@�C�@�o@���@���@�ff@���@�@���@��@�p�@�&�@���@���@���@�r�@�Q�@�b@��@��F@�C�@�"�@���@���@���@���@��\@�ff@�E�@�-@�J@�@��@��T@��#@���@���@�p�@�7L@��@�V@���@�r�@�Z@�Z@�Z@�Z@�Q�@�b@��@�C�@�33@�
=@�ȴ@���@��@�p�@�X@�?}@��@�A�@�1'@��@���@�S�@��@��R@�~�@�^5@�V@�V@�5?@��h@�p�@�X@��`@�I�@��@\)@
=@~�@~��@~�+@~@}�-@}�@}O�@}�@|�@{�m@{33@z��@zn�@yG�@x�`@x��@xĜ@x�u@x�u@xQ�@xb@w�@w�P@v��@vff@v{@u��@u�h@uO�@t��@t�@s��@r��@q��@qG�@p�@o�;@o;d@n�R@nV@m@m`B@l�@l�D@lI�@l1@k�m@k�F@k"�@j�@i�7@hĜ@h �@g\)@g�@fv�@f@e�@eV@d�j@dj@dj@dz�@dz�@dz�@dz�@c��@cdZ@c@bn�@a�#@a�7@a�@`�u@`Q�@` �@`  @_�;@_�@_\)@^ȴ@]�-@]V@\�@\z�@\z�@\1@[S�@Z�\@ZM�@Y��@Y7L@Y�@Y%@Y%@X��@X�@X �@W�@W|�@Wl�@WK�@V�@VV@U�h@Up�@U`B@U?}@T��@T��@T�/@T��@Tz�@S��@S��@S�@St�@S"�@R�@R�H@R��@RM�@RJ@Q��@Q�7@Q�@P�9@P�@Pr�@PQ�@O�;@OK�@O�@N�y@Nȴ@N�+@N{@M�T@M��@M�-@M�@MV@L�D@L9X@K��@Kt�@Ko@J��@Jn�@JJ@I�#@I��@I%@H�9@H�@HA�@H  @G|�@G;d@Fȴ@F��@Fv�@FE�@F$�@E�@E@EO�@E�@D�/@D9X@C��@CdZ@B�@B��@B^5@B=q@BJ@A�^@Ahs@A&�@A%@@�u@@b@@  @?�@?��@?+@?+@?�@>��@>�R@>v�@=�@=�-@=��@=�@<��@<�@;�
@;ƨ@;��@;t�@;S�@;"�@:��@:��@:~�@:M�@:J@9�#@9x�@9G�@9�@8��@8��@8��@8A�@8 �@8b@8b@7�@7�@7;d@6ȴ@6ff@5�@5�-@5/@4��@4�D@4j@4I�@4(�@3�m@3�F@3��@3�@3t�@3t�@3dZ@333@3@2�\@2~�@2n�@2=q@1��@1hs@1�@0��@0�`@0�`@0Ĝ@0�9@0�u@0�@0r�@0A�@0 �@0b@/�@/�;@/��@/\)@.�@.��@.ff@-�-@-p�@-`B@-O�@-?}@-?}@-?}@-/@-�@,�j@,I�@+ƨ@+dZ@*�H@*^5@*J@)�#@)�^@)��@)x�@)hs@)X@)%@(��@(Ĝ@(��@(r�@(  @'�P@'l�@'K�@'+@&�y@&�+@&ff@&V@&$�@%�@%@%p�@%?}@%V@$��@$�j@$�@$z�@#�m@#�@#o@"��@"�!@"��@"~�@"=q@"J@!��@!�^@!x�@!G�@!7L@ ��@ ��@ Ĝ@ �u@ �u@ �@ bN@ 1'@   @�w@\)@+@
=@��@��@�y@�@ff@�@�h@p�@`B@��@��@��@j@I�@9X@�@1@1@1@�
@�@S�@33@@��@�\@-@�@�^@G�@%@��@Ĝ@�u@r�@Q�@  @��@�w@�P@l�@l�@l�@l�@;d@�y@��@��@��@�+@ff@ff@V@5?@{@�T@�@p�@`B@?}@/@V@��@�D@z�@j@Z@(�@�@�m@��@��@�@t�@S�@@��@~�@~�@n�@M�@�@J@�#@�^@��@�7@x�@X@%@��@��@�u@r�@bN@A�@�@�@|�@;d@�y@��@ff@5?@�@��@@�h@`B@�@�j@�@z�@Z@9X@9X@��@�
@�F@��@dZ@C�@
�H@
^5@
J@	��@	x�@	G�@	�@��@��@Ĝ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�O�A�(�A��A��A��A�v�Aש�A��/A�{A��A�$�A��A��A�&�A�1'A�=qA�A�A�Q�A�XA�&�Aח�A�1AԍPAӗ�AҋDA��`Aћ�A���A�;dA�  A�"�A�ƨA�ƨA�O�A�v�A�-A�A��AʃA���A��#A�/A�9XAƸRA�C�A��A�x�A�{A�?}A�1AøRA�A�A��wA�`BA�v�A�dZA��A�=qA�=qA��;A�bNA�A���A�Q�A�C�A�bA���A�7LA��#A�K�A���A��jA�C�A��DA�^5A�v�A��
A�I�A��
A�bNA�7LA�"�A�C�A�G�A��A�VA�ffA��A��FA���A�p�A��A�`BA�ffA�-A�A�A���A�\)A�+A���A�1'A�I�A���A�O�A�z�A�  A��A���A�1A�9XA�A��+A�%A�^5A��FA��FA�E�A���A���A��A��`A��RA��Ap�A}�
AzAx(�AvA�As
=Am�Ai�^Af�Ad=qAa��A`jA^�yA\ĜA\�A\bA\9XA[�mAZ�AY�
AY"�AW33AU�AS�mAR�AQ��AP�9AN�uAMx�AL�AK��AJ=qAF�`AD~�AC\)AB�RAB{AA��AA;dA@1A=ƨA;��A:v�A9��A9
=A8ȴA7�A7�TA7��A6-A5C�A2�9A1�A1;dA0�A,ĜA,A+p�A*�uA'��A&�A&n�A&-A%�TA%
=A"��A"��A"�uA"�A �jA {A   Al�AbAI�A7LA�wA�A$�A�A�AbA�A��A~�A�wAl�AZAVAbAjA-A�A�wAoA	��A�A^5A�FA
=A ĜA �uA M�@�S�@�@�=q@���@��@�7L@��!@��@��
@�o@�J@���@�%@�r�@�b@�@�@�R@�-@��/@�bN@@�Z@�;d@�@�\@�n�@�V@�J@�O�@��/@�9X@�;d@�?}@�A�@�ȴ@��@�{@��@�@�%@�1'@�"�@�{@�9X@���@��/@�33@թ�@�n�@Ѓ@ύP@�33@�n�@�-@��@̣�@�1'@�  @��
@ˍP@�;d@�o@ʰ!@��T@�hs@�%@ȣ�@��m@�dZ@�=q@�hs@�%@�j@���@Å@��H@°!@�v�@��^@¸R@�-@�S�@���@���@��/@�Ĝ@�ȴ@�z�@���@���@��@���@�j@�1@���@�@��@���@�ff@�=q@��!@�@��@�%@�Q�@��@��P@�
=@�ȴ@��\@�@�p�@�&�@��@���@���@��H@�n�@���@�V@�t�@�n�@�@���@��@��9@�z�@�Q�@�A�@�1'@� �@��@���@�33@���@��@���@�^5@��@�X@�/@�V@��9@��@�r�@�I�@�(�@���@���@�|�@�S�@�@�{@���@���@��@�x�@�hs@�hs@�hs@�%@��9@��@��@�ƨ@��w@���@�o@��\@��@��-@�x�@�?}@��9@�A�@��@�1@��m@���@��P@�l�@�K�@�o@��H@���@���@��+@�v�@�-@���@���@��7@�p�@�O�@�&�@��`@�j@��@��
@�t�@���@�~�@�^5@��@��@��@��T@��T@��#@���@���@��h@�?}@�V@���@���@�r�@�9X@�1@��F@�l�@�C�@�o@���@���@�ff@���@�@���@��@�p�@�&�@���@���@���@�r�@�Q�@�b@��@��F@�C�@�"�@���@���@���@���@��\@�ff@�E�@�-@�J@�@��@��T@��#@���@���@�p�@�7L@��@�V@���@�r�@�Z@�Z@�Z@�Z@�Q�@�b@��@�C�@�33@�
=@�ȴ@���@��@�p�@�X@�?}@��@�A�@�1'@��@���@�S�@��@��R@�~�@�^5@�V@�V@�5?@��h@�p�@�X@��`@�I�@��@\)@
=@~�@~��@~�+@~@}�-@}�@}O�@}�@|�@{�m@{33@z��@zn�@yG�@x�`@x��@xĜ@x�u@x�u@xQ�@xb@w�@w�P@v��@vff@v{@u��@u�h@uO�@t��@t�@s��@r��@q��@qG�@p�@o�;@o;d@n�R@nV@m@m`B@l�@l�D@lI�@l1@k�m@k�F@k"�@j�@i�7@hĜ@h �@g\)@g�@fv�@f@e�@eV@d�j@dj@dj@dz�@dz�@dz�@dz�@c��@cdZ@c@bn�@a�#@a�7@a�@`�u@`Q�@` �@`  @_�;@_�@_\)@^ȴ@]�-@]V@\�@\z�@\z�@\1@[S�@Z�\@ZM�@Y��@Y7L@Y�@Y%@Y%@X��@X�@X �@W�@W|�@Wl�@WK�@V�@VV@U�h@Up�@U`B@U?}@T��@T��@T�/@T��@Tz�@S��@S��@S�@St�@S"�@R�@R�H@R��@RM�@RJ@Q��@Q�7@Q�@P�9@P�@Pr�@PQ�@O�;@OK�@O�@N�y@Nȴ@N�+@N{@M�T@M��@M�-@M�@MV@L�D@L9X@K��@Kt�@Ko@J��@Jn�@JJ@I�#@I��@I%@H�9@H�@HA�@H  @G|�@G;d@Fȴ@F��@Fv�@FE�@F$�@E�@E@EO�@E�@D�/@D9X@C��@CdZ@B�@B��@B^5@B=q@BJ@A�^@Ahs@A&�@A%@@�u@@b@@  @?�@?��@?+@?+@?�@>��@>�R@>v�@=�@=�-@=��@=�@<��@<�@;�
@;ƨ@;��@;t�@;S�@;"�@:��@:��@:~�@:M�@:J@9�#@9x�@9G�@9�@8��@8��@8��@8A�@8 �@8b@8b@7�@7�@7;d@6ȴ@6ff@5�@5�-@5/@4��@4�D@4j@4I�@4(�@3�m@3�F@3��@3�@3t�@3t�@3dZ@333@3@2�\@2~�@2n�@2=q@1��@1hs@1�@0��@0�`@0�`@0Ĝ@0�9@0�u@0�@0r�@0A�@0 �@0b@/�@/�;@/��@/\)@.�@.��@.ff@-�-@-p�@-`B@-O�@-?}@-?}@-?}@-/@-�@,�j@,I�@+ƨ@+dZ@*�H@*^5@*J@)�#@)�^@)��@)x�@)hs@)X@)%@(��@(Ĝ@(��@(r�@(  @'�P@'l�@'K�@'+@&�y@&�+@&ff@&V@&$�@%�@%@%p�@%?}@%V@$��@$�j@$�@$z�@#�m@#�@#o@"��@"�!@"��@"~�@"=q@"J@!��@!�^@!x�@!G�@!7L@ ��@ ��@ Ĝ@ �u@ �u@ �@ bN@ 1'@   @�w@\)@+@
=@��@��@�y@�@ff@�@�h@p�@`B@��@��@��@j@I�@9X@�@1@1@1@�
@�@S�@33@@��@�\@-@�@�^@G�@%@��@Ĝ@�u@r�@Q�@  @��@�w@�P@l�@l�@l�@l�@;d@�y@��@��@��@�+@ff@ff@V@5?@{@�T@�@p�@`B@?}@/@V@��@�D@z�@j@Z@(�@�@�m@��@��@�@t�@S�@@��@~�@~�@n�@M�@�@J@�#@�^@��@�7@x�@X@%@��@��@�u@r�@bN@A�@�@�@|�@;d@�y@��@ff@5?@�@��@@�h@`B@�@�j@�@z�@Z@9X@9X@��@�
@�F@��@dZ@C�@
�H@
^5@
J@	��@	x�@	G�@	�@��@��@Ĝ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	�B	�B	��B	��B	�#B	��B
B
1B
	7B
DB
hB
�B
�B
 �B
+B
=qB
YB
�1B
�wBffBffB^5BiyBl�B|�By�B��B��Bo�BgmBr�B��B��B��B�9B��B��B��B�3B�
B�jBB��B�ZB�B��BB	7BuB �B �B#�B+B2-B8RBG�BN�B[#BdZB� B�uB|�BaHB]/B[#BZBe`BffBdZBz�B�1B{�B~�By�B{�Bx�Bp�BW
BH�B>wB1'B�B{BVBDB1B1B1B  B��B�B�;B�BŢB�3B��Bm�Bq�B}�Bm�B_;BYBP�BM�B[#B_;BT�BK�B;dB �B+B
��B
�B
�`B
ǮB
�jB
�LB
�9B
�B
��B
��B
�hB
�B
n�B
_;B
R�B
;dB
PB	�HB	��B	�wB	��B	��B	�VB	�B	�=B	�+B	�JB	�bB	�7B	{�B	u�B	iyB	XB	O�B	H�B	A�B	=qB	2-B	(�B	!�B	�B	�B	+B��B�B�B�B�B�sB�`B�)B��BȴBƨBĜBĜBƨBǮBȴBÖBÖB�RB�LB�9B�-B��B��B��B��B�hB�VB�JB�JB�bB��B��B��B��B�-B�LB�}B��B��B��B��B��B��B�FB�}B��B�
B�B�#B�
B��B��B��B��B�
B��B��B��B�
B�
B��BƨB�B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�3BB��B��B��B�B�
B�B�B�)B�;B�5B�;B�;B�;B�BB�HB�`B�BB�fB�mB�yB�B�B�B�B�B�B�B�B�B�ZB�TB�ZB�ZB�fB�sB�mB�B��B��B��B��B	B	B	+B	DB	\B	{B	�B	�B	"�B	"�B	"�B	"�B	#�B	#�B	#�B	"�B	!�B	!�B	#�B	'�B	+B	1'B	8RB	;dB	8RB	9XB	:^B	8RB	7LB	6FB	7LB	:^B	>wB	G�B	K�B	K�B	K�B	I�B	H�B	VB	\)B	\)B	]/B	e`B	e`B	gmB	hsB	iyB	k�B	n�B	n�B	m�B	l�B	m�B	m�B	m�B	o�B	r�B	s�B	s�B	t�B	t�B	u�B	s�B	t�B	z�B	|�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�1B	�DB	�PB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�9B	�?B	�?B	�XB	�XB	�^B	�}B	ÖB	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�HB	�TB	�TB	�TB	�TB	�ZB	�`B	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
1B
	7B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
-B
-B
.B
.B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
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
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
8RB
8RB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
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
?}B
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
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
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
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
YB
YB
YB
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
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
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
cTB
dZB
dZB
dZB
dZB
dZB
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
ffB
ffB
ffB
ffB
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
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�[B	�AB	�B	�B	��B	��B	� B	��B	��B
�B
1B
	7B
)B
NB
�B
�B
 �B
+6B
>BB
Z�B
��B
��BgBh>B_�BjeBn/B}�B{B��B�yBq�BhXBr�B�B�TB�B��B��B�qB�6B�9B��B��BÖB��B�FB�/B��BB
�BMB!|B!�B%�B-B3hB9�BI�BO�B\]BfB�AB�9B~�BbB^B\�B\�Bf�Bg�Bf�B|B�RB}"B�oB}B}qBz�BuZBY�BKDBA B5%BdBSB�BdB	RB
�BB[B�xB�/B�B�]B��B�	B��Bp�BsMB�Bo B`\BZ�BQ�BN"B\�B`�BWsBO�B@4B#�B	7B
�"B
��B
��B
�B
�"B
�B
�B
��B
��B
��B
�B
�7B
qAB
bhB
WsB
AUB
 B	�,B	��B	� B	�B	��B	�bB	��B	��B	�zB	�B	��B	��B	}VB	x8B	k�B	Y�B	QhB	JrB	CGB	?�B	3�B	*�B	#:B	B	KB		�B�<B�B�B�qB�B�eB�$BޞBөB��B�_B�SBňB��BȀBʦB�SB�tB��B��B�zB��B��B��B��B��B�TB�B��B�B��B��B�!B� B��B�nB�8B�4B�[B�MB�2B�vB��B�$B�2B��B��B�sBیB��B��B��B��BӏBԯB�yB��B�[BԕB��BخB��B�0B�B��B�sB�
B��B�9B�SB�B�SB��B��B��B�dB��B��B�xB�]B�	B�)B�B��B��B��BBЗB҉BԯB�YB�yB��B�eB�xB�VBބBߊB߾B��B��B�4B�B�-B�8B��B�B�B��B�-B�|B�|B�B��B��B��B�B��B�LB�B�B��B�
B�B�`B�8B�0B�.B	GB	�B	�B	xB	�B	B	B	B	#:B	#nB	#TB	#�B	$ZB	$@B	$ZB	#TB	"NB	"NB	$&B	($B	+kB	1B	9	B	<�B	9	B	:*B	:�B	8�B	8�B	7�B	7�B	:�B	?}B	H�B	L0B	LJB	L~B	JXB	HKB	VB	\�B	\CB	]/B	e�B	e�B	g�B	iB	i�B	k�B	o B	n�B	m�B	mB	m�B	m�B	nB	pUB	s3B	s�B	tB	utB	utB	v�B	tnB	u%B	{0B	}�B	}<B	~(B	�B	�;B	�'B	�-B	�aB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�B	�,B	�B	�2B	�fB	��B	�cB	�OB	�AB	�MB	�TB	�nB	�ZB	��B	��B	��B	��B	��B	ðB	��B	�B	�#B	�6B	�B	�B	� B	�[B	�SB	�$B	�EB	�EB	�KB	�7B	�7B	�WB	�qB	�xB	�dB	�OB	�OB	�pB	ߊB	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�	B	�$B	�*B	�B	�*B	�B	�B	�"B	�B	�BB	�B	�.B
UB
gB
MB
mB
EB
�B
	�B
	RB
	lB

rB
xB
^B
^B
^B
dB
dB
�B
�B
jB
�B
�B
pB
pB
�B
�B
}B
}B
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
#B
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
!B
�B
 B
 B
!-B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"B
#:B
"�B
"�B
#B
#B
#�B
$&B
$&B
%B
%FB
&2B
&2B
'RB
'8B
($B
($B
)*B
)DB
)DB
*KB
*B
+B
+6B
+6B
+B
+QB
,qB
-]B
-wB
.}B
.}B
/OB
/iB
/OB
0UB
1vB
1AB
1[B
1'B
1AB
1AB
1'B
1[B
1[B
2aB
2|B
2aB
2aB
3hB
3hB
4nB
4TB
4TB
5tB
5ZB
5ZB
5tB
5�B
6�B
7�B
7fB
8lB
8lB
7�B
8�B
8�B
8�B
9�B
9�B
9�B
9XB
9XB
9rB
9�B
:�B
:�B
:�B
:�B
:xB
:�B
:�B
;�B
;B
;�B
;B
;B
;dB
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
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
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
J	B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
KB
LB
K�B
K�B
K�B
K�B
L�B
L�B
MB
L�B
MB
MB
M�B
NB
M�B
N"B
N"B
OB
N�B
OB
N�B
N�B
O�B
O�B
PB
PB
PB
PB
O�B
O�B
Q B
Q B
Q B
QB
QB
QB
RB
Q�B
RB
RB
R B
R B
R B
S&B
S&B
SB
T,B
T,B
TB
U2B
U2B
UB
UB
UB
T�B
UB
UB
T�B
U2B
UB
VB
VB
VB
V9B
VB
VB
WYB
W$B
W?B
W$B
W
B
X+B
XB
XEB
X+B
X+B
XEB
XEB
XB
XEB
XEB
X+B
X_B
YeB
Y1B
YKB
ZkB
Z7B
[#B
[#B
[#B
[#B
[=B
[WB
[=B
[qB
[qB
\xB
\]B
\xB
]dB
]dB
^jB
^OB
^OB
^jB
^OB
^OB
^OB
_VB
_VB
_pB
_VB
_pB
_VB
`vB
`\B
`\B
`vB
`\B
abB
a|B
abB
abB
abB
abB
b�B
b�B
b�B
bhB
b�B
bhB
b�B
c�B
c�B
d�B
dtB
dtB
dtB
d�B
ezB
ezB
e�B
ezB
ezB
f�B
f�B
f�B
f�B
f�B
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
jB
jB
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
xB
w�B
w�B
xB
x�B
y	B
x�B
x�B
y	B
y�B
y�B
zB
y�B
y�B
y�B
zB
z�B
{B
z�B
{B
{B
{B
|B
|B
|B
}B
}B
}B
}B
}"B
~B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201706160032182017061600321820170616003218201806221314462018062213144620180622131446201804050716272018040507162720180405071627  JA  ARFMdecpA19c                                                                20170612093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170612003542  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170612003543  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170612003543  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170612003544  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170612003544  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170612003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170612003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170612003544  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170612003545                      G�O�G�O�G�O�                JA  ARUP                                                                        20170612010940                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170612153649  CV  JULD            G�O�G�O�F�s�                JM  ARGQJMQC2.0                                                                 20170612153649  CV  JULD_LOCATION   G�O�G�O�F�s�                JM  ARGQJMQC2.0                                                                 20170612153649  CV  LATITUDE        G�O�G�O�A��                JM  ARGQJMQC2.0                                                                 20170612153649  CV  LONGITUDE       G�O�G�O��#	7                JM  ARCAJMQC2.0                                                                 20170615153218  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170615153218  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221627  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041446  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                