CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-10T00:35:16Z creation;2017-09-10T00:35:20Z conversion to V3.1;2019-12-19T07:57:57Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20170910003516  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_158                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�$�;���1   @�$��[ @4\c�A \�d��g��	1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A��A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D���D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��R@��A (�A�\A<��A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cn�Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=De �Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�D�y�D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�y�DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D���D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�3�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AᛦA��A៾AᝲAᗍA�+A�A�A�A�|�A�n�A�p�A�p�A�l�A�jA�M�A�A�A�oA��/A��A�ZAݑhA�ffA�VA�I�A�+A�(�A�\)A؇+A�O�A��AёhA�^5A�;dAΟ�A�M�AͬAɋDA�VAǴ9A��AÅA���A�^5A�+A��FA���A��#A�VA���A���A�JA�+A���A��#A��A��HA��9A��7A���A�bA�1'A�&�A�{A���A�A�oA�VA�VA��FA���A���A�M�A���A��#A�z�A�I�A��9A�%A�r�A�?}A�ffA���A�ĜA�l�A��^A��A���A��A��FA�~�A�Q�A���A��;A�v�A���A�7LA�-A�l�A�%A�5?A�\)A�/A��A��#A��uA��A��^A�-A�-A���A���A�hsA��RA�ƨA�^A}�FAz��Ax�Ax1'Av��As�Ap��ApM�Ao`BAmt�Ak��Ai/Ag��Af��Ae��Ad��Ad1Acx�Ab�A^�+A\-AZ �AY��AX��AX^5AV�HAUK�AS33AQC�AO�7AN��AM�AM�AMl�AL��AK��AJv�AG&�AEt�AEVAD��AC�PAA�A=��A;%A9G�A8ZA7�A6��A5�TA5VA4r�A3�A1|�A0��A.�\A,�HA+��A(9XA&ȴA%�mA%/A#�wA"ĜA"5?A ��A��A1'A/A1A;dAjA1A��A�A�A��A��A+A"�A��Al�A�A�#AC�A{A��An�AA
�A
bA	�A��A$�AȴA%AJA�-A�`A�TA\)A �A @��-@�\)@�@��@�ȴ@��+@�=q@�{@���@��@���@�Z@�@�=q@�/@�C�@��@�\@�A�@�=q@�7L@�I�@�w@�l�@��y@���@�R@�@���@��@ؼj@� �@��@���@�E�@�G�@�  @Ѻ^@���@�1@ϝ�@���@�(�@�+@�ȴ@�v�@�G�@�A�@ǅ@�\)@�l�@���@�V@�Q�@��m@�  @�  @�C�@�@�5?@��#@��h@�&�@�Ĝ@��;@��P@�
=@��@��j@�\)@�@���@���@�~�@�$�@���@� �@��!@�E�@��T@���@�1@��F@��@��@�@���@�I�@��@��m@��@�C�@��@�ȴ@�=q@��@�@�hs@�G�@�%@���@��`@�A�@���@�|�@��@�ƨ@���@�1@��@�ƨ@���@�o@�@�n�@�@�?}@��D@�(�@��@�1@���@��m@��m@�ƨ@���@�K�@��@��@�ȴ@��R@��!@�^5@��@���@�x�@�V@��u@�9X@��w@�|�@�C�@�33@�
=@��@���@�v�@�E�@�@���@���@�X@�%@��@��D@��@�r�@�Q�@�(�@���@�ƨ@���@�;d@��y@���@�~�@�-@��-@�p�@�/@�V@���@�(�@��m@���@���@�S�@�
=@���@�5?@��@��h@�7L@��@�Ĝ@�Q�@� �@�1@���@���@��@�S�@�"�@�$�@��#@�@���@��h@��@�p�@�`B@�X@��@���@�r�@��F@���@��P@�S�@��@�ȴ@��!@���@���@�V@�=q@��@�@���@���@���@��7@�X@��@���@�z�@�bN@�A�@�1'@�b@���@��@��
@��w@��@��@�C�@�o@�v�@�-@�{@��@��-@��h@�p�@�/@���@��u@�Q�@�A�@�9X@�1'@�(�@�1@��;@�ƨ@���@���@�t�@�33@��@��@�@���@�^5@�-@�{@��@���@���@�p�@�G�@�&�@���@���@��D@�bN@�b@��@��m@���@���@�\)@�"�@�+@�
=@���@��\@�^5@�-@���@��T@���@��-@���@��h@�X@�&�@���@���@�I�@�  @��
@�ƨ@��w@��P@�\)@�33@�ȴ@�V@�{@�@���@��@���@���@��@�?}@�&�@���@��`@��/@���@��9@�z�@�9X@�(�@�1@��@�@~ȴ@~��@~5?@}O�@|��@|z�@{ƨ@{��@{C�@z^5@y��@yx�@y�@x�`@w�@w
=@v�@v�R@v�+@vff@up�@t�D@t(�@s�m@s��@sC�@so@r�@r~�@q�@qhs@q7L@pĜ@pb@o;d@n�R@nE�@m��@m�h@m`B@l�@lz�@k��@j�H@jn�@j�@i�^@i&�@h��@h�@hbN@hA�@g�@g\)@g+@f�y@f�+@fff@fV@f5?@e@e`B@d�@cƨ@co@b��@b^5@bM�@bJ@a�@a��@a��@ahs@a%@`r�@`1'@_�@_l�@^{@]�@]O�@]?}@]?}@]?}@]V@]/@\�@\�@[�@[33@Z��@Zn�@Z^5@ZJ@Y��@X��@X��@X�@W�w@W+@V��@Vȴ@V�R@U�T@U`B@U`B@UO�@UO�@U/@T�@T(�@S��@St�@SS�@SC�@S"�@R�H@R��@R~�@R=q@RJ@Q��@Q7L@Q%@P�`@PĜ@P�@P1'@O�@O�w@OK�@O
=@N��@M�T@M�@M/@M�@L��@L�/@L�@L(�@K�F@J��@J~�@Jn�@J=q@J-@J�@I�@I��@H�`@H�@G�;@G��@G�w@G|�@Gl�@GK�@G�@F��@F��@Fȴ@F��@Fv�@FV@F@E@E�h@E�@E�@Ep�@E?}@D�@Dz�@Dj@D�@C�m@C�F@C�@CC�@C@B�!@A��@AX@A�@A�@A�@A%@@Ĝ@@bN@?�w@?\)@?
=@>�y@>��@>v�@>5?@>@=��@=?}@=V@<��@<�/@<Z@;�
@;S�@;@:��@:M�@:=q@:�@9�@9��@9��@9X@97L@97L@9&�@9%@8�9@8�@8Q�@7��@7|�@7+@6��@6��@6v�@6E�@6$�@5��@5�h@5O�@4�@4�D@4z�@4j@4Z@4Z@4�@3�F@333@2��@2n�@2�@1��@1�@1�#@1�^@1�^@1�^@1��@1x�@1hs@1G�@0��@0A�@/�@/��@/l�@/K�@/�@/
=@.��@.�y@.ȴ@.��@.��@.V@.$�@-�T@-��@-O�@-/@,��@,��@,�@,�D@,�D@,Z@,1@+��@+C�@+o@*�@*��@*��@*^5@*J@)�^@)�7@)7L@)&�@)�@(�`@(��@(r�@( �@(  @'�@'l�@&�y@&��@&V@&5?@%�T@%@%�@%�@$�@$z�@$I�@$(�@#��@#��@#C�@#o@#@"�H@"�!@"M�@"-@"�@!�^@!hs@!7L@!�@ ��@ ��@ ��@ �u@ Q�@  �@   @   @   @�w@�P@\)@;d@
=@�y@��@5?@@��@�-@�@/@��@��@j@j@Z@Z@9X@(�@�m@��@dZ@C�@"�@@��@�!@��@�\@~�@n�@M�@�@�@��@hs@x�@hs@7L@&�@%@��@�`@�`@�`@��@Ĝ@�9@�@A�@b@��@��@K�@��@�@��@��@V@{@�T@@��@�h@V@�D@j@I�@(�@��@��@t�@t�@S�@o@�@��@�!@��@�\@~�@=q@�@��@�^@hs@�@%@�`@��@r�@ �@��@\)@�@�@
=@��@ff@@�h@p�@O�@?}@?}@?}@/@V@�j@j@Z@Z@(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AᛦA��A៾AᝲAᗍA�+A�A�A�A�|�A�n�A�p�A�p�A�l�A�jA�M�A�A�A�oA��/A��A�ZAݑhA�ffA�VA�I�A�+A�(�A�\)A؇+A�O�A��AёhA�^5A�;dAΟ�A�M�AͬAɋDA�VAǴ9A��AÅA���A�^5A�+A��FA���A��#A�VA���A���A�JA�+A���A��#A��A��HA��9A��7A���A�bA�1'A�&�A�{A���A�A�oA�VA�VA��FA���A���A�M�A���A��#A�z�A�I�A��9A�%A�r�A�?}A�ffA���A�ĜA�l�A��^A��A���A��A��FA�~�A�Q�A���A��;A�v�A���A�7LA�-A�l�A�%A�5?A�\)A�/A��A��#A��uA��A��^A�-A�-A���A���A�hsA��RA�ƨA�^A}�FAz��Ax�Ax1'Av��As�Ap��ApM�Ao`BAmt�Ak��Ai/Ag��Af��Ae��Ad��Ad1Acx�Ab�A^�+A\-AZ �AY��AX��AX^5AV�HAUK�AS33AQC�AO�7AN��AM�AM�AMl�AL��AK��AJv�AG&�AEt�AEVAD��AC�PAA�A=��A;%A9G�A8ZA7�A6��A5�TA5VA4r�A3�A1|�A0��A.�\A,�HA+��A(9XA&ȴA%�mA%/A#�wA"ĜA"5?A ��A��A1'A/A1A;dAjA1A��A�A�A��A��A+A"�A��Al�A�A�#AC�A{A��An�AA
�A
bA	�A��A$�AȴA%AJA�-A�`A�TA\)A �A @��-@�\)@�@��@�ȴ@��+@�=q@�{@���@��@���@�Z@�@�=q@�/@�C�@��@�\@�A�@�=q@�7L@�I�@�w@�l�@��y@���@�R@�@���@��@ؼj@� �@��@���@�E�@�G�@�  @Ѻ^@���@�1@ϝ�@���@�(�@�+@�ȴ@�v�@�G�@�A�@ǅ@�\)@�l�@���@�V@�Q�@��m@�  @�  @�C�@�@�5?@��#@��h@�&�@�Ĝ@��;@��P@�
=@��@��j@�\)@�@���@���@�~�@�$�@���@� �@��!@�E�@��T@���@�1@��F@��@��@�@���@�I�@��@��m@��@�C�@��@�ȴ@�=q@��@�@�hs@�G�@�%@���@��`@�A�@���@�|�@��@�ƨ@���@�1@��@�ƨ@���@�o@�@�n�@�@�?}@��D@�(�@��@�1@���@��m@��m@�ƨ@���@�K�@��@��@�ȴ@��R@��!@�^5@��@���@�x�@�V@��u@�9X@��w@�|�@�C�@�33@�
=@��@���@�v�@�E�@�@���@���@�X@�%@��@��D@��@�r�@�Q�@�(�@���@�ƨ@���@�;d@��y@���@�~�@�-@��-@�p�@�/@�V@���@�(�@��m@���@���@�S�@�
=@���@�5?@��@��h@�7L@��@�Ĝ@�Q�@� �@�1@���@���@��@�S�@�"�@�$�@��#@�@���@��h@��@�p�@�`B@�X@��@���@�r�@��F@���@��P@�S�@��@�ȴ@��!@���@���@�V@�=q@��@�@���@���@���@��7@�X@��@���@�z�@�bN@�A�@�1'@�b@���@��@��
@��w@��@��@�C�@�o@�v�@�-@�{@��@��-@��h@�p�@�/@���@��u@�Q�@�A�@�9X@�1'@�(�@�1@��;@�ƨ@���@���@�t�@�33@��@��@�@���@�^5@�-@�{@��@���@���@�p�@�G�@�&�@���@���@��D@�bN@�b@��@��m@���@���@�\)@�"�@�+@�
=@���@��\@�^5@�-@���@��T@���@��-@���@��h@�X@�&�@���@���@�I�@�  @��
@�ƨ@��w@��P@�\)@�33@�ȴ@�V@�{@�@���@��@���@���@��@�?}@�&�@���@��`@��/@���@��9@�z�@�9X@�(�@�1@��@�@~ȴ@~��@~5?@}O�@|��@|z�@{ƨ@{��@{C�@z^5@y��@yx�@y�@x�`@w�@w
=@v�@v�R@v�+@vff@up�@t�D@t(�@s�m@s��@sC�@so@r�@r~�@q�@qhs@q7L@pĜ@pb@o;d@n�R@nE�@m��@m�h@m`B@l�@lz�@k��@j�H@jn�@j�@i�^@i&�@h��@h�@hbN@hA�@g�@g\)@g+@f�y@f�+@fff@fV@f5?@e@e`B@d�@cƨ@co@b��@b^5@bM�@bJ@a�@a��@a��@ahs@a%@`r�@`1'@_�@_l�@^{@]�@]O�@]?}@]?}@]?}@]V@]/@\�@\�@[�@[33@Z��@Zn�@Z^5@ZJ@Y��@X��@X��@X�@W�w@W+@V��@Vȴ@V�R@U�T@U`B@U`B@UO�@UO�@U/@T�@T(�@S��@St�@SS�@SC�@S"�@R�H@R��@R~�@R=q@RJ@Q��@Q7L@Q%@P�`@PĜ@P�@P1'@O�@O�w@OK�@O
=@N��@M�T@M�@M/@M�@L��@L�/@L�@L(�@K�F@J��@J~�@Jn�@J=q@J-@J�@I�@I��@H�`@H�@G�;@G��@G�w@G|�@Gl�@GK�@G�@F��@F��@Fȴ@F��@Fv�@FV@F@E@E�h@E�@E�@Ep�@E?}@D�@Dz�@Dj@D�@C�m@C�F@C�@CC�@C@B�!@A��@AX@A�@A�@A�@A%@@Ĝ@@bN@?�w@?\)@?
=@>�y@>��@>v�@>5?@>@=��@=?}@=V@<��@<�/@<Z@;�
@;S�@;@:��@:M�@:=q@:�@9�@9��@9��@9X@97L@97L@9&�@9%@8�9@8�@8Q�@7��@7|�@7+@6��@6��@6v�@6E�@6$�@5��@5�h@5O�@4�@4�D@4z�@4j@4Z@4Z@4�@3�F@333@2��@2n�@2�@1��@1�@1�#@1�^@1�^@1�^@1��@1x�@1hs@1G�@0��@0A�@/�@/��@/l�@/K�@/�@/
=@.��@.�y@.ȴ@.��@.��@.V@.$�@-�T@-��@-O�@-/@,��@,��@,�@,�D@,�D@,Z@,1@+��@+C�@+o@*�@*��@*��@*^5@*J@)�^@)�7@)7L@)&�@)�@(�`@(��@(r�@( �@(  @'�@'l�@&�y@&��@&V@&5?@%�T@%@%�@%�@$�@$z�@$I�@$(�@#��@#��@#C�@#o@#@"�H@"�!@"M�@"-@"�@!�^@!hs@!7L@!�@ ��@ ��@ ��@ �u@ Q�@  �@   @   @   @�w@�P@\)@;d@
=@�y@��@5?@@��@�-@�@/@��@��@j@j@Z@Z@9X@(�@�m@��@dZ@C�@"�@@��@�!@��@�\@~�@n�@M�@�@�@��@hs@x�@hs@7L@&�@%@��@�`@�`@�`@��@Ĝ@�9@�@A�@b@��@��@K�@��@�@��@��@V@{@�T@@��@�h@V@�D@j@I�@(�@��@��@t�@t�@S�@o@�@��@�!@��@�\@~�@=q@�@��@�^@hs@�@%@�`@��@r�@ �@��@\)@�@�@
=@��@ff@@�h@p�@O�@?}@?}@?}@/@V@�j@j@Z@Z@(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B]/B\)B]/B]/B\)B\)B\)B\)B[#BZBYBYBXBVBS�BO�BJ�BA�B2-BbB
�B
��B�B�B �BoB
�B
��B
�oB
v�B
jB
iyB
�^B
�NBBl�B�1B��B�/B��B�B�B+B�B!�B,B>wB\)BcTB\)BW
BM�BQ�BP�BD�B<jB(�B.BA�BF�BT�BffBcTBcTBl�Bm�BgmBXB@�B2-B�BB��B�yB�sB�TB�#B��B��BĜB�^B��B�=B�%Bz�Bl�BiyBaHB]/BYBN�B?}B(�B$�B9XB=qB-B(�BE�B-B%�B+B'�B�B	7B
�5B
�
B
��B
�'B
�{B
�+B
�B
p�B
^5B
L�B
6FB
�B
%B
B	�B	��B	��B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	�oB	�JB	� B	cTB	_;B	VB	ZB	R�B	L�B	>wB	49B	)�B	�B	�B	�B	�B	�B	�B	\B��B��B�5B�;B�fB�;B��BB�RB�9B�LB�LB�dB�3B�!B�!B�!B��B��B��B��B��B�oB|�B�%B�1B�1B� B� B�Bw�Bv�Bq�Bu�Bv�Bv�Bu�By�Bx�Br�Br�Bm�Bt�B{�B~�Bv�Bk�Bu�B�B|�Bt�Bw�Bp�Bs�Bn�Bk�BdZBdZBaHB]/B\)B^5BffBcTBdZBk�Bn�BjBhsBk�Bw�Bz�B{�Bz�B{�B{�Bz�Bw�Br�Bu�Bq�Bv�Br�Bm�Bk�BffBdZBcTBm�Bl�Bn�Bp�Bp�Br�Bp�BhsB]/B[#Be`Bq�Bp�Bw�Bt�Bp�Bo�Bl�Bs�Bu�Bw�Bt�Bn�Bz�B�B�B~�B�B�7B�hB�oB�oB�VB��B��B��B�'B�9B�LB�dB�qB�wB�wBÖBĜBȴBǮBŢBɺB��B��B�
B�B�B��B��B��B��B�;B�HB�NB�yB�B�B�B�B��B	+B		7B		7B		7B	PB	hB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	.B	1'B	33B	9XB	?}B	B�B	D�B	E�B	I�B	N�B	M�B	O�B	P�B	Q�B	S�B	XB	ZB	ZB	[#B	[#B	[#B	[#B	^5B	bNB	ffB	iyB	jB	jB	iyB	k�B	o�B	r�B	t�B	w�B	}�B	� B	�B	�1B	�=B	�7B	�=B	�DB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�-B	�9B	�RB	�XB	�dB	�dB	�qB	��B	��B	B	��B	B	B	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�5B	�;B	�NB	�NB	�TB	�TB	�NB	�ZB	�`B	�`B	�`B	�sB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
%B
%B
+B
+B
+B
1B
1B
1B
	7B
1B
1B
	7B
	7B
	7B
	7B

=B
JB
JB
JB
DB
DB
DB

=B

=B
DB
PB
PB
PB
PB
\B
bB
bB
hB
hB
oB
oB
oB
oB
oB
hB
uB
uB
uB
uB
uB
{B
uB
oB
uB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
$�B
%�B
%�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
&�B
&�B
&�B
%�B
&�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
)�B
(�B
-B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
33B
33B
2-B
33B
49B
33B
33B
5?B
5?B
5?B
49B
5?B
7LB
7LB
7LB
7LB
6FB
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
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
<jB
=qB
>wB
>wB
>wB
>wB
=qB
=qB
=qB
@�B
@�B
@�B
A�B
@�B
@�B
@�B
?}B
@�B
A�B
C�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
D�B
D�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
L�B
K�B
L�B
L�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
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
VB
VB
W
B
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
_;B
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
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
ffB
ffB
ffB
ffB
ffB
gmB
ffB
gmB
gmB
hsB
gmB
gmB
gmB
gmB
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
k�B
k�B
jB
jB
jB
jB
k�B
k�B
k�B
k�B
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
m�B
m�B
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
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
o�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
w�B
w�B
x�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B]/B\CB]/B]IB\CB\)B\)B\)B[=BZ7BYBYBX+BV9BT,BPBK^BBuB4BuB
�5B
�B�B B!�BB
��B
�4B
��B
z^B
n�B
lqB
�B
�:B�Bp�B�BB��B�pB��B�B��BKBKB#:B.}B@�B^�BeFB^�BZBO�BS[BR�BF�B>�B+�B0�BCGBH�BW$Bh�Be�Be�Bn�Bo�BjeB[�BDMB5?B�B�B�*B�6B�DB�&BܬBյBԯB�EB��B��B�B�+B|�Bn/BjBb�B]�BY�BO�BA�B,�B'RB9XB>]B/ B)yBG�B2aB'�B+kB(�B!-B�B
�&B
�B
�B
��B
��B
��B
��B
r�B
`�B
O(B
8�B
!�B
�B
[B	��B	�sB	�aB	��B	��B	��B	�_B	��B	�zB	�&B	�B	��B	��B	��B	�[B	g�B	bB	XEB	Z�B	TB	M�B	@�B	6�B	,�B	B	�B	�B	�B	B	�B	HB	 �B�2B��B�-B�B�BB�B�%B��B�fB�XB��B�6B��B�vB�[B�AB��B��B��B��B��B��B��B��B��B�lB��B�oB�9By�BxlBs�Bw2Bx8BxBv�BzxBy�Bt9BtBoiBvB|�B�Bx�Bn�BwLB��B~BvFBx�BrGBt�BpBl�Be�Be,Bb�B_VB^jB_�BgBd�Be�BlWBo�Bk�BjBl�BxB{B|B{0B|6B|6B{By$Bt9Bv�Br�BwLBs�BoBmCBhXBe�Bd�BncBmCBo Bp�Bq'Br�BqBi�B_VB]�Bf�BrBqvBxButBq�Bp�Bm�BtTBvzBxRBu�BpUB{B�aB�uB�B��B��B��B��B�&B��B�B�B�
B�[B��B��B��B��B��B��B�B�9B�B�KBƨB��B͹B�2B�?B�9B�9B�{B��B��B�BߊB�B� B�B�B�B�MB�B��B	zB		�B		�B		�B	�B	�B	�B	�B	�B	�B	B	�B	�B	�B	B	IB	"hB	./B	1AB	3B	9rB	?�B	B�B	D�B	FB	J#B	O(B	NpB	PbB	QNB	RoB	TFB	XEB	ZQB	ZQB	[=B	[=B	[qB	[qB	^jB	b�B	f�B	i�B	j�B	j�B	i�B	k�B	o�B	r�B	u%B	x8B	~]B	��B	�SB	��B	�XB	�lB	�rB	�xB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�8B	�>B	�DB	�WB	�iB	�GB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	� B	�.B	�&B	�:B	�&B	�TB	бB	�9B	�1B	�1B	�7B	�QB	�QB	�QB	�7B	�eB	�_B	�kB	ٚB	�dB	�VB	�jB	ߊB	�B	�B	�nB	�nB	�B	�tB	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�'B	�B	��B	�B	��B	��B	�B	�B	�B	�	B	�$B	�B	��B	�B	�B	�B	�B	�"B	�"B	�"B	�"B	�<B	�(B	��B	�B	�(B	�<B	�B
 B
 B
 4B
-B
[B
;B
;B
UB
AB
;B
AB
[B
MB
MB
3B
aB
gB
mB
+B
_B
YB
tB
zB
_B
_B
KB
KB
KB
	lB
fB
�B
	�B
	lB
	�B
	�B

�B
~B
~B
dB
�B
xB
�B

�B

�B
�B
jB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
B
�B
 B
 �B
 �B
!B
!B
!B
!-B
"B
#B
#B
# B
#�B
$�B
&B
&B
%,B
%B
&B
&B
&B
'B
(
B
($B
'B
'8B
'8B
&2B
'RB
)*B
*B
*B
*0B
*0B
*0B
*0B
*B
*KB
*0B
+6B
+QB
*KB
)yB
-CB
./B
/5B
/B
/5B
/OB
0;B
0oB
0�B
0oB
0UB
0UB
1AB
1[B
3MB
3�B
2�B
3MB
4TB
3�B
3�B
5tB
5tB
5ZB
4�B
5tB
7fB
7LB
7fB
7fB
6�B
5�B
5tB
6zB
7�B
7�B
7fB
7fB
7fB
8�B
8�B
8�B
8�B
8�B
9�B
:�B
:�B
:�B
:�B
;B
;�B
;�B
;B
;�B
;�B
<�B
=�B
>wB
>�B
>�B
>�B
=�B
=�B
=�B
@�B
@�B
@�B
A�B
@�B
@�B
@�B
?�B
@�B
A�B
C�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
D�B
D�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
IB
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
M�B
MB
K�B
MB
MB
OB
N�B
PB
O�B
Q B
QB
QB
QB
QB
QB
RB
QB
Q B
Q B
QB
QB
Q4B
RB
RB
SB
SB
S&B
S&B
S&B
SB
S&B
TB
S&B
T�B
T�B
UB
T�B
U2B
U2B
TFB
T,B
U2B
U2B
V9B
W
B
W
B
W$B
W$B
W$B
W
B
W$B
W$B
W?B
W$B
VSB
VSB
W$B
X+B
YKB
Y1B
Y1B
Z7B
ZB
Z7B
Z7B
ZQB
Z7B
Y1B
Z7B
Z7B
Z7B
ZQB
[=B
[=B
[=B
\CB
\]B
\CB
\]B
[WB
[WB
\CB
]dB
]dB
]dB
]IB
]IB
]IB
^OB
^jB
_pB
_;B
_pB
_VB
_VB
_VB
_VB
`\B
_VB
_pB
_pB
`vB
abB
a|B
abB
bhB
b�B
b�B
b�B
c�B
c�B
c�B
cnB
cnB
c�B
d�B
e`B
dtB
d�B
d�B
e�B
ezB
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
f�B
g�B
g�B
hsB
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
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
m�B
m�B
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
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
o�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
w�B
w�B
y	B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709140033432017091400334320170914003343201806221318572018062213185720180622131857201804050721212018040507212120180405072121  JA  ARFMdecpA19c                                                                20170910093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170910003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170910003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170910003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170910003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170910003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170910003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170910003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170910003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170910003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20170910005556                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170910153556  CV  JULD            G�O�G�O�F�'�                JM  ARCAJMQC2.0                                                                 20170913153343  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170913153343  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222121  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041857  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                