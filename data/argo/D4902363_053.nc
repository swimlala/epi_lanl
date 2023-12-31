CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-02T00:35:33Z creation;2016-11-02T00:35:35Z conversion to V3.1;2019-12-19T08:26:27Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20161102003533  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               5A   JA  I2_0576_053                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @����+<�1   @���K� @:�F
�L0�d�Vl�!1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C\�C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D��Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DI��DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D¹�D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D׀RD׽D��D�@RD�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�y�D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�/A�-A�-A�+A�+A�-A�-A�-A�-A�-A�-A�-A�/A�/A�/A�/A�1'A�/A�(�A�"�A� �A� �A�VA��A�n�A�C�A�$�A�p�A˴9A�ZAɧ�A���A�`BA�G�AA��A�%A�XA�|�A��A��wA��A�{A��A�9XA�$�A���A��uA�ffA���A���A��PA��FA�+A�1A��yA��HA��A�"�A�r�A�A�A���A���A�ȴA�n�A�ȴA�A�A���A�M�A��DA�-A���A�ȴA���A�jA�p�A�5?A��A��A��-A��HA�M�A��!A�-A�G�A��^A�VA�A���A���A���A�ȴA���A�|�A��PA��PA��A���A��A�A�ȴA���A�/A��/A�XA��A�+A�=qAA~�\A|�`A{�
Az�9AyAw��Av$�At�RAs�Aq��Aqp�Ap��Ap�ApA�Ao�-Ao%An�jAn�\An$�Am33AlbAk;dAj^5Ah�uAe��Ac�Aa�hA^�A]�wA\��AX�`AWVAV��AV�AU�AS�AR~�ARA�AQ�AQ%AP�jAP�uAO�AOp�AOK�AOK�AOC�AN�AM��AMƨAM��AMt�ALA�AJ�AIoAG��AGXAE�mAE+AD��ADbNAB��AA�-A@�`A?�TA>�jA<��A;\)A:5?A9�A8$�A7K�A7VA6�HA6��A6�\A5
=A3S�A2 �A0{A/hsA/�A/%A.��A.��A-�FA-G�A,�/A,E�A+�A+�hA*�A*JA)A(bNA'�;A&�A&ZA%�#A%�A%33A$��A#�TA#;dA"�!A"n�A!��A z�A�A��A�#AoA+A$�AbA��A\)A��A�DA  A33AZAA�mAJAz�A��At�A
�uA�RA�A�`A��A�yAQ�A��AS�A V@��!@�?}@��@��R@��@���@�@���@��@�A�@�K�@��@�S�@�G�@�9X@��@��@�p�@�9X@�\)@�\@�1'@�=q@�1@���@�ff@�{@�/@�+@���@��
@׾w@׶F@׮@�
=@�p�@Ӆ@���@�+@���@�\)@�-@�7L@�z�@��@�
=@ư!@�$�@��@���@�(�@�K�@�
=@��y@�v�@�x�@�Z@��@�M�@���@���@�z�@��w@�=q@���@��@�Ĝ@�9X@���@���@�+@�M�@�G�@��@��m@�+@��+@��#@��/@��@�C�@�ff@��@���@��u@��F@�+@���@�^5@���@�|�@���@���@�=q@�@���@��7@��@�9X@���@���@�O�@��@��
@��F@��T@��/@�Q�@��;@�;d@�
=@�@���@�z�@� �@� �@��;@���@�ƨ@�
=@�~�@��@���@��@��7@�p�@��-@��@��@���@��T@���@�X@���@���@�l�@�@�J@�7L@��@���@��9@��D@�j@�r�@�j@�Q�@��@���@��;@���@�C�@���@��m@��m@��;@���@�t�@�l�@�K�@�;d@���@�n�@�=q@���@��h@��@�?}@�V@��/@�bN@�(�@��
@�\)@��@�~�@�-@��7@�&�@��j@�bN@�A�@��@���@��P@�l�@��@�@��H@���@���@�~�@�M�@�5?@�J@��#@��-@��@�O�@�7L@�7L@�7L@�&�@���@�z�@�I�@�  @��@�P@�P@l�@+@~v�@}�-@}V@|��@{ƨ@{S�@{33@z�!@z~�@z^5@z=q@x��@x1'@x �@x �@x1'@xA�@xA�@xA�@x �@w��@w�P@w;d@w;d@v�R@u�-@tz�@s�@sS�@so@r�\@r-@qx�@qX@qG�@q&�@p�9@p�u@pQ�@p1'@pQ�@pQ�@pbN@pQ�@p �@o��@o�w@o�P@n��@n��@nv�@m@l�@k�m@ko@j�@j�@kC�@l�D@l�/@l(�@k��@kt�@kt�@k��@k��@kC�@j�@j^5@i��@h�`@g��@g�P@g\)@g�@f@f@e�@e/@d�/@d�D@dZ@c�@b��@b-@b�@b�@b^5@b~�@b^5@a�@ax�@aG�@a7L@a&�@`��@`Q�@_�w@_\)@_K�@_K�@_+@_
=@^��@^$�@]�@]@]�@\�@\�D@\1@[��@[C�@[C�@[S�@Z�@Z��@ZM�@ZJ@Y��@Y�@Y��@Y��@Yx�@Y7L@Y&�@Y�@Y%@X��@X��@Xr�@Xr�@XbN@X1'@W�;@W�@W\)@W;d@W+@W�@V�y@V�R@V5?@U��@UO�@T�@T�@T�j@T�j@T�@TZ@T�@S�
@S�@So@R�@R��@R�!@R��@R^5@Q7L@P�@P1'@P �@Pb@P  @O�@O\)@O;d@N�@N�+@Nv�@NE�@M�T@M�-@Mp�@M?}@MV@L��@LZ@L(�@L1@K�
@KS�@J��@J�\@J^5@J�@I�#@I�7@I7L@H�`@H��@HbN@HA�@H  @G|�@F��@Fȴ@Fff@E�@E@E��@EV@DI�@C��@CdZ@CS�@C@B��@B~�@A��@A��@Ax�@A%@@��@@1'@@  @?�;@?��@?�w@?�P@?+@>�R@>ff@>V@>V@>$�@=@=?}@<�@<1@;��@;S�@;C�@;o@:��@:~�@9�#@9hs@9�@9%@8�u@8A�@7�;@7�@6�y@6��@6v�@6V@6E�@6$�@5�T@5�h@4�@4�@4z�@4(�@41@3��@3�F@3dZ@3"�@2�H@2�!@2�\@2�\@2^5@2M�@2-@1��@1hs@1�@0��@0��@0�u@0bN@0A�@0A�@0A�@0Q�@01'@/�;@/��@/��@/l�@/
=@.��@.��@.v�@.ff@.E�@.{@-�T@-��@-O�@,��@,�D@,(�@+�m@+ƨ@+ƨ@+�F@+��@+S�@*~�@*-@)�@)��@)�7@)G�@(�`@(�@(A�@'��@'|�@'\)@';d@'�@'
=@&�y@&�R@&ff@&$�@%�@%��@%@%��@%�@%p�@%`B@%?}@%?}@%V@$I�@#��@#��@#�m@#�
@#�@"n�@"J@!��@!x�@!G�@ ��@ �9@ ��@ r�@ bN@ Q�@ 1'@�;@�P@;d@�R@V@@�T@�T@��@@��@�@�@�@�@p�@/@��@z�@9X@��@�m@ƨ@dZ@33@��@�!@~�@M�@=q@��@��@��@hs@�`@Ĝ@��@�u@�u@�@r�@bN@A�@ �@��@�@��@|�@\)@�@�+@V@5?@@�-@��@�h@�h@�@p�@`B@`B@?}@�/@�@z�@Z@9X@(�@�@�m@ƨ@�F@��@S�@C�@33@"�@o@"�@o@o@o@�H@��@�\@^5@�@�@��@X@7L@�`@��@�u@Q�@ �@�@��@K�@�@�@�+@E�@$�@��@`B@O�@?}@/@V@�@��@�j@�D@j@1@��@33@o@
��@
�!@
��@
~�@
^5@
=q@
�@	�@	�@	��@	G�@��@�9@��@�u@r�@r�@r�@bN@bN@Q�@A�@1'@b@�@+@
=@ȴ@��@�+@v�@E�@$�@$�@$�@{@@@@@��@�h@p�@p�@p�@O�@�@V@�@�/@�/@��@�j@�D@9X@�m@�@"�@��@~�@~�@~�@^5@=q@-@-@-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�/A�-A�-A�+A�+A�-A�-A�-A�-A�-A�-A�-A�/A�/A�/A�/A�1'A�/A�(�A�"�A� �A� �A�VA��A�n�A�C�A�$�A�p�A˴9A�ZAɧ�A���A�`BA�G�AA��A�%A�XA�|�A��A��wA��A�{A��A�9XA�$�A���A��uA�ffA���A���A��PA��FA�+A�1A��yA��HA��A�"�A�r�A�A�A���A���A�ȴA�n�A�ȴA�A�A���A�M�A��DA�-A���A�ȴA���A�jA�p�A�5?A��A��A��-A��HA�M�A��!A�-A�G�A��^A�VA�A���A���A���A�ȴA���A�|�A��PA��PA��A���A��A�A�ȴA���A�/A��/A�XA��A�+A�=qAA~�\A|�`A{�
Az�9AyAw��Av$�At�RAs�Aq��Aqp�Ap��Ap�ApA�Ao�-Ao%An�jAn�\An$�Am33AlbAk;dAj^5Ah�uAe��Ac�Aa�hA^�A]�wA\��AX�`AWVAV��AV�AU�AS�AR~�ARA�AQ�AQ%AP�jAP�uAO�AOp�AOK�AOK�AOC�AN�AM��AMƨAM��AMt�ALA�AJ�AIoAG��AGXAE�mAE+AD��ADbNAB��AA�-A@�`A?�TA>�jA<��A;\)A:5?A9�A8$�A7K�A7VA6�HA6��A6�\A5
=A3S�A2 �A0{A/hsA/�A/%A.��A.��A-�FA-G�A,�/A,E�A+�A+�hA*�A*JA)A(bNA'�;A&�A&ZA%�#A%�A%33A$��A#�TA#;dA"�!A"n�A!��A z�A�A��A�#AoA+A$�AbA��A\)A��A�DA  A33AZAA�mAJAz�A��At�A
�uA�RA�A�`A��A�yAQ�A��AS�A V@��!@�?}@��@��R@��@���@�@���@��@�A�@�K�@��@�S�@�G�@�9X@��@��@�p�@�9X@�\)@�\@�1'@�=q@�1@���@�ff@�{@�/@�+@���@��
@׾w@׶F@׮@�
=@�p�@Ӆ@���@�+@���@�\)@�-@�7L@�z�@��@�
=@ư!@�$�@��@���@�(�@�K�@�
=@��y@�v�@�x�@�Z@��@�M�@���@���@�z�@��w@�=q@���@��@�Ĝ@�9X@���@���@�+@�M�@�G�@��@��m@�+@��+@��#@��/@��@�C�@�ff@��@���@��u@��F@�+@���@�^5@���@�|�@���@���@�=q@�@���@��7@��@�9X@���@���@�O�@��@��
@��F@��T@��/@�Q�@��;@�;d@�
=@�@���@�z�@� �@� �@��;@���@�ƨ@�
=@�~�@��@���@��@��7@�p�@��-@��@��@���@��T@���@�X@���@���@�l�@�@�J@�7L@��@���@��9@��D@�j@�r�@�j@�Q�@��@���@��;@���@�C�@���@��m@��m@��;@���@�t�@�l�@�K�@�;d@���@�n�@�=q@���@��h@��@�?}@�V@��/@�bN@�(�@��
@�\)@��@�~�@�-@��7@�&�@��j@�bN@�A�@��@���@��P@�l�@��@�@��H@���@���@�~�@�M�@�5?@�J@��#@��-@��@�O�@�7L@�7L@�7L@�&�@���@�z�@�I�@�  @��@�P@�P@l�@+@~v�@}�-@}V@|��@{ƨ@{S�@{33@z�!@z~�@z^5@z=q@x��@x1'@x �@x �@x1'@xA�@xA�@xA�@x �@w��@w�P@w;d@w;d@v�R@u�-@tz�@s�@sS�@so@r�\@r-@qx�@qX@qG�@q&�@p�9@p�u@pQ�@p1'@pQ�@pQ�@pbN@pQ�@p �@o��@o�w@o�P@n��@n��@nv�@m@l�@k�m@ko@j�@j�@kC�@l�D@l�/@l(�@k��@kt�@kt�@k��@k��@kC�@j�@j^5@i��@h�`@g��@g�P@g\)@g�@f@f@e�@e/@d�/@d�D@dZ@c�@b��@b-@b�@b�@b^5@b~�@b^5@a�@ax�@aG�@a7L@a&�@`��@`Q�@_�w@_\)@_K�@_K�@_+@_
=@^��@^$�@]�@]@]�@\�@\�D@\1@[��@[C�@[C�@[S�@Z�@Z��@ZM�@ZJ@Y��@Y�@Y��@Y��@Yx�@Y7L@Y&�@Y�@Y%@X��@X��@Xr�@Xr�@XbN@X1'@W�;@W�@W\)@W;d@W+@W�@V�y@V�R@V5?@U��@UO�@T�@T�@T�j@T�j@T�@TZ@T�@S�
@S�@So@R�@R��@R�!@R��@R^5@Q7L@P�@P1'@P �@Pb@P  @O�@O\)@O;d@N�@N�+@Nv�@NE�@M�T@M�-@Mp�@M?}@MV@L��@LZ@L(�@L1@K�
@KS�@J��@J�\@J^5@J�@I�#@I�7@I7L@H�`@H��@HbN@HA�@H  @G|�@F��@Fȴ@Fff@E�@E@E��@EV@DI�@C��@CdZ@CS�@C@B��@B~�@A��@A��@Ax�@A%@@��@@1'@@  @?�;@?��@?�w@?�P@?+@>�R@>ff@>V@>V@>$�@=@=?}@<�@<1@;��@;S�@;C�@;o@:��@:~�@9�#@9hs@9�@9%@8�u@8A�@7�;@7�@6�y@6��@6v�@6V@6E�@6$�@5�T@5�h@4�@4�@4z�@4(�@41@3��@3�F@3dZ@3"�@2�H@2�!@2�\@2�\@2^5@2M�@2-@1��@1hs@1�@0��@0��@0�u@0bN@0A�@0A�@0A�@0Q�@01'@/�;@/��@/��@/l�@/
=@.��@.��@.v�@.ff@.E�@.{@-�T@-��@-O�@,��@,�D@,(�@+�m@+ƨ@+ƨ@+�F@+��@+S�@*~�@*-@)�@)��@)�7@)G�@(�`@(�@(A�@'��@'|�@'\)@';d@'�@'
=@&�y@&�R@&ff@&$�@%�@%��@%@%��@%�@%p�@%`B@%?}@%?}@%V@$I�@#��@#��@#�m@#�
@#�@"n�@"J@!��@!x�@!G�@ ��@ �9@ ��@ r�@ bN@ Q�@ 1'@�;@�P@;d@�R@V@@�T@�T@��@@��@�@�@�@�@p�@/@��@z�@9X@��@�m@ƨ@dZ@33@��@�!@~�@M�@=q@��@��@��@hs@�`@Ĝ@��@�u@�u@�@r�@bN@A�@ �@��@�@��@|�@\)@�@�+@V@5?@@�-@��@�h@�h@�@p�@`B@`B@?}@�/@�@z�@Z@9X@(�@�@�m@ƨ@�F@��@S�@C�@33@"�@o@"�@o@o@o@�H@��@�\@^5@�@�@��@X@7L@�`@��@�u@Q�@ �@�@��@K�@�@�@�+@E�@$�@��@`B@O�@?}@/@V@�@��@�j@�D@j@1@��@33@o@
��@
�!@
��@
~�@
^5@
=q@
�@	�@	�@	��@	G�@��@�9@��@�u@r�@r�@r�@bN@bN@Q�@A�@1'@b@�@+@
=@ȴ@��@�+@v�@E�@$�@$�@$�@{@@@@@��@�h@p�@p�@p�@O�@�@V@�@�/@�/@��@�j@�D@9X@�m@�@"�@��@~�@~�@~�@^5@=q@-@-@-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�qB�wB�wB�wB�wB�wB�wB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�wB�}B��BǮBŢB��B��B��B��B��B��B�#B�B�B�B�fB�NB�B�B�5BȴBÖBBǮBǮBɺB��BɺBŢB�^B�B�B�?BǮB�jB��B�%Bx�Be`B]/B^5BA�B,B$�B�B+B�B�HB�B��B�qB��B�hB^5BT�BQ�BI�B7LB)�B�BbB+BB�B6FBXBYB[#BJ�B8RB5?B2-B.B#�B�BDB
��B
�B
�B
�B
�`B
�HB
�B
��B
ĜB
�XB
��B
��B
��B
�bB
�7B
� B
s�B
iyB
aHB
VB
K�B
F�B
A�B
>wB
<jB
9XB
33B
1'B
/B
+B
"�B
�B
PB
B	�B	��B	�}B	�B	��B	�hB	�7B	|�B	l�B	k�B	k�B	dZB	hsB	m�B	n�B	o�B	l�B	jB	jB	iyB	gmB	ffB	e`B	e`B	e`B	aHB	_;B	^5B	\)B	ZB	Q�B	K�B	D�B	C�B	;dB	8RB	6FB	49B	/B	(�B	"�B	�B	{B	\B	%B��B��B��B�B�B�B�B�B�B�TB�BB�B��B��B��B��B��B��B��BɺBƨBĜBB��B�jB�RB�9B�-B�B�B�B��B��B��B��B��B��B��B��B��B�uB�\B�DB�7B�B}�B|�B|�Bu�Bq�Bn�Bl�BiyBgmBbNB]/BZBR�BP�BN�BM�BI�BE�BF�BD�B@�B>wB<jB9XB6FB49B33B0!B/B.B,B)�B(�B(�B&�B%�B"�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBhBhBbB\B\B\BbBbBbBVBhBVB\BVBVBVBVB\BVB\B\B\BbBhBhBhBhBhBoBhB�B�B�B�B�B�B�B�B�B�B�B�B �B"�B$�B%�B&�B(�B)�B-B0!B33B5?B6FB6FB9XB<jB?}BA�BD�BE�BF�BJ�BL�BL�BM�BO�BP�BP�BT�BT�BW
B[#B`BB^5BaHBcTBjBl�Bl�Bn�Bp�Bp�Bu�Bw�B}�B}�B}�B� B�B�B�B�%B�7B�DB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�9B�LB�XB�^B�jB�qB�qB�qB�}B��BĜBȴB��B��B��B��B��B��B��B��B�B�B�5B�HB�NB�ZB�fB�sB�B�B�B��B��B��B��B	B	1B	JB	\B	bB	hB	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	&�B	(�B	)�B	,B	.B	/B	0!B	0!B	1'B	5?B	8RB	:^B	=qB	>wB	?}B	?}B	?}B	A�B	C�B	F�B	H�B	J�B	O�B	R�B	S�B	W
B	XB	XB	YB	]/B	`BB	aHB	bNB	bNB	bNB	cTB	dZB	ffB	ffB	ffB	gmB	hsB	jB	m�B	o�B	s�B	t�B	u�B	w�B	x�B	{�B	}�B	}�B	~�B	�B	�B	�B	�%B	�=B	�DB	�PB	�PB	�\B	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�3B	�9B	�XB	�dB	�jB	�wB	�wB	�wB	�wB	�}B	�}B	�}B	�}B	��B	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�;B	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
VB
VB
\B
\B
bB
bB
bB
hB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
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
1'B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
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
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
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
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
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
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
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
XB
XB
XB
XB
YB
YB
YB
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
^5B
^5B
^5B
^5B
^5B
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
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
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
dZB
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
ffB
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
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
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
k�B
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�qB�wB�wB�wB�wB�wB�wB�qB�qB�qB�qB�qB��B�qB�qB�qB�qB�qB��B��B�qB��B��B�.B�B�MB��B��B��B�jB�}B�EB��BּB�B��B��B��B�0B�,B�AB�aB�B�DB�mBĜBȚB�B��B�B̈́B�1B�B�;B��B�`B�rB� B��B��B~Bj�BaBcnBDgB-�B&�B�B
#B�AB�B��B��B�OB��B�mB_VBU�BS�BK�B9$B+kBB�B	B9B+B6�BX�BZ�B^�BLdB8�B6FB4�B0UB%�B)BB �B
�TB
�/B
�B
�fB
�B
ٴB
�bB
ƨB
��B
�B
��B
�B
� B
�DB
��B
u�B
kkB
cB
WYB
L�B
G_B
BB
>�B
="B
:*B
3�B
1�B
/�B
,=B
$&B
�B
�B
�B	�B	��B	�'B	��B	�~B	��B	�PB	~�B	mCB	l�B	mB	fLB	i�B	nB	o�B	p;B	mB	kB	k6B	jB	g�B	f�B	e�B	e�B	fLB	a�B	_�B	_B	^B	\B	S�B	M6B	E�B	E9B	<PB	9$B	72B	6B	0�B	*0B	$ZB	�B	�B	B	�B	 iB�B��B�B��B�B�B�B�B�FB�hB��B�aB�&B�:BѷB�B�pB�xB�rB�_B�SBðB��B��B�XB�%B�hB��B��B��B��B��B�$B��B��B�jB��B�B��B�2B��B��B�xB�tB~�B~BB�Bw�BsMBo�Bm�Bj�Bi_Bd&B_�B[�BS�BQ�BP}BPBK)BF�BG�BFBA�B@ B?B:�B7�B5tB4nB1'B0�B/�B,�B*KB)�B)�B'�B'�B#�B$B"�B \BpB�B�BxBqB#B	B�B?B�B�BMB�BoB�B�B�B�BB�B�B�BBoB�B.B�B�B�B�B�B�BB�B�B�B�B�B�BTB:B&B�B$B�B�BQB�BOBB5B!B BB B BB!|B#�B%zB&fB'�B)�B*�B-�B0�B3�B5�B6�B6�B:DB=B@ BA�BESBF�BGzBK)BM6BMPBN<BP.BQNBQ�BUgBU�BXB\)BaB^�Ba�Bd�BkQBl�BmBo5BqBqvBv+Bx�B~(B~(B~BB�OB�;B��B��B��B�lB�xB��B��B��B��B��B��B��B�B�nB��B�,B�B��B��B�yB�=B�OB�aB�hB�nB�fB��B��B��B��B��B��B��B��BāB��B��B�B�B�B�4B�:B�oB�aB�_BچBބB�|B�B�B�B��B��B��B�-B�?B�DB�<B��B	mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$B	%B	'B	)DB	*KB	,WB	./B	/OB	0;B	0oB	1vB	5�B	8�B	:�B	=�B	>�B	?�B	?�B	?�B	A�B	C�B	F�B	H�B	K)B	PB	S&B	T,B	W?B	X+B	XEB	Y�B	]dB	`BB	abB	bhB	bNB	bhB	cnB	dtB	f�B	f�B	f�B	g�B	h�B	j�B	m�B	pB	s�B	t�B	u�B	xB	y	B	|B	~B	~B	.B	� B	�AB	�MB	�?B	�XB	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	��B	��B	��B	��B	��B	��B	�iB	�UB	�AB	�-B	�3B	�nB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	ĶB	��B	��B	�B	�B	��B	��B	��B	��B	�B	�1B	�QB	�qB	�]B	�IB	�IB	�dB	�~B	�pB	�B	�nB	�nB	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�nB	�ZB	�tB	�zB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�	B	�B	��B	�B	��B	�B	�6B	�"B	�B	�.B
 B
 B
 B
;B
 B
AB
'B
GB
gB
SB
SB
?B
YB
YB
�B
�B
	lB
	RB
	RB
	lB
	RB

XB

rB
xB
xB
dB
dB
dB
jB
jB
�B
�B
�B
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
B
�B
�B
�B
�B
�B
�B
 B
 �B
 �B
!B
!�B
#B
#�B
#�B
#�B
$B
#�B
%,B
%,B
&B
%�B
%�B
%�B
'8B
'8B
'8B
(XB
)*B
)B
*B
*0B
*0B
*0B
+QB
+QB
,"B
,=B
,=B
-)B
-CB
.IB
/OB
/OB
0;B
0;B
0;B
0UB
1[B
1[B
1vB
3MB
3MB
3MB
3hB
3hB
4nB
4nB
4TB
5ZB
5ZB
5ZB
6`B
6zB
6zB
6`B
6`B
7�B
8lB
8lB
8lB
9rB
9rB
9rB
9XB
9XB
9rB
9�B
9�B
:^B
:�B
:�B
:�B
;B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
?�B
?}B
?�B
?�B
?�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
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
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
IB
J	B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
MB
MB
M�B
N�B
O(B
O�B
O�B
P�B
Q B
P�B
Q B
Q B
Q B
Q B
Q B
P�B
Q B
QB
QB
RB
R B
RB
R�B
S&B
S&B
T,B
T,B
U2B
U2B
U2B
U2B
UB
V9B
V9B
VB
VSB
VB
W$B
W$B
W
B
W
B
W$B
W
B
W?B
W?B
XEB
X+B
X+B
X+B
Y1B
YKB
Y1B
Z7B
ZQB
ZQB
ZQB
[=B
[#B
[=B
[=B
[#B
[=B
[#B
[=B
[=B
\CB
\CB
\CB
\]B
]IB
]IB
]dB
]dB
]/B
]dB
]IB
^OB
^5B
^OB
^5B
^OB
^5B
^OB
^5B
^OB
^OB
^OB
^jB
_VB
_VB
_VB
_pB
_VB
_pB
_VB
_pB
`\B
`vB
`vB
`vB
abB
a|B
a|B
abB
b�B
bhB
b�B
cnB
cnB
cTB
cnB
cnB
c�B
d�B
dtB
d�B
d�B
d�B
e�B
ezB
ezB
ezB
f�B
ffB
f�B
f�B
f�B
f�B
g�B
gmB
g�B
g�B
g�B
h�B
h�B
hsB
hsB
hsB
hsB
hsB
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611060036492016110600364920161106003649201806221216152018062212161520180622121615201804050409082018040504090820180405040908  JA  ARFMdecpA19c                                                                20161102093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161102003533  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161102003534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161102003534  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161102003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161102003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161102003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161102003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161102003535  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161102003535                      G�O�G�O�G�O�                JA  ARUP                                                                        20161102013152                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161102153705  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161105153649  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161105153649  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190908  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031615  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                