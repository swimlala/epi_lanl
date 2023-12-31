CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-11T00:35:22Z creation;2018-04-11T00:35:30Z conversion to V3.1;2019-12-19T07:45:07Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180411003522  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_228                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Z5��ހ1   @�Z6����@:���-��dl($x1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D˼�D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ D�|�D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_=qBg=qBo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C>�C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CT�CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C|�C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D��Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(��D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˹�D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D׀RD׽D���D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�y�D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��/A��;A��;A��/A��HA��HA��HA��;A��;A��;A��TA��TA��`A��`A��mA��yA��A��mA��`A��mA��`A���A���A��wA��A�+A��HA���A���A�M�A���A�p�A�{A��mA���A�  A�1A��A��!A�~�A���A�n�A���A���A���A��TA�ZA���A�z�A��;A�(�A�
=A��A��A��/A���A���A�\)A�bA���A���A�t�A���A���A��yA�
=A�bA��A�1A��#A��A�?}A�A�Q�A���A��9A�\)A�  A���A�A�A��A��wA�1A���A��A~��A~^5A~Q�Az�/Aw�7Av�Au��Au"�At�RAt(�As�hAsK�Ar��Ar(�AqhsAo�Al�RAk��Ak/Aj�RAi��Ai;dAi�AhjAf�!Ae�hAe�Ad�Ac��Abr�Ab$�AaXA`JA_oA^�!A]dZA\��A[�hAY�TAX�9AX1'AW�-AW+AV1'AU`BAS�ARE�AP=qAN�AL��AL��ALbAJ�9AIXAH��AH�DAH5?AG�AF�AFZAE��AEG�AEoAD�AD��ADr�AD-AD1'AD1'AD �AD �AD �AC�AC|�AC%ABM�A?�#A=�TA;�wA;
=A:{A8��A8JA7��A6��A5dZA4=qA3��A3C�A2�A2�A2�A1A0��A/XA-�^A-\)A,��A,��A,JA*��A)�PA)K�A)&�A(��A'\)A&$�A$�yA$�DA#hsA!hsA �A+A��A�!A  A~�A�A�A�TA��A33A��A�^A+A��A�`AE�A�-A$�A�!AbNA  A�;A�A�AJAt�AA
r�A
 �A	�wA�DA�TA�`A=qAVA �A��A;dAjA��A �!A ZA 5?@�ƨ@�l�@���@���@�x�@� �@�@��`@��@�@���@� �@�;d@�R@�G�@�F@��/@�R@�@�z�@�~�@��@�b@�=q@ߥ�@�~�@��@��m@�X@�9X@�;d@�V@�J@թ�@�V@Դ9@�Q�@� �@Ӯ@Ӆ@�K�@���@�-@Ь@ϥ�@���@˕�@ʇ+@�V@ǍP@�-@��@��@�V@�1@���@��9@��m@��H@�M�@��@���@��@�O�@�&�@�%@��9@�ƨ@�K�@��@��@�
=@���@���@��\@��^@�|�@��!@��\@��@��-@�x�@�hs@���@��@�Z@��m@���@�5?@�z�@�dZ@��R@���@�X@��@�9X@���@��\@�X@�j@��@�S�@�+@��@�n�@�1@�-@��#@���@�x�@�O�@�V@��`@��@�I�@�Q�@�bN@� �@�  @�t�@�|�@�33@���@��H@�ȴ@���@��\@�-@��7@���@�o@���@��@��y@��@��R@�=q@���@�`B@�z�@�A�@� �@�b@�  @���@���@���@��P@��P@��P@�\)@���@�5?@�-@��@�J@��@���@���@�z�@�Z@��m@��@�+@�@���@��@�33@�+@�o@��@�~�@�^5@��@��#@�hs@�&�@��`@�I�@��w@�+@��@��@�o@�@��H@���@���@�v�@�M�@�-@�$�@��@�J@��@��^@���@�`B@��@��u@�1'@��w@�C�@��y@��!@�J@���@��j@�r�@�I�@�9X@� �@�@�P@\)@K�@~�y@~$�@|�/@|��@|j@{�
@{�@{C�@z��@y��@yx�@yG�@xĜ@xA�@x  @w\)@w;d@w|�@w\)@vV@u`B@t��@t�j@s�m@r�H@rM�@q��@q��@q&�@p�`@pr�@o��@o�P@o|�@o;d@n�@n��@nff@m��@mO�@m�@l�/@l�j@l�@l��@lz�@lj@lj@lj@lj@lZ@l(�@l1@k�
@kt�@kdZ@kdZ@kS�@ko@j��@j�\@j^5@j-@i�@iX@hQ�@g�@g�P@gl�@g;d@f�@f�+@f@e/@d��@d�@d�/@d�@dI�@c��@cƨ@c��@c�@ct�@cS�@c"�@b�H@b��@b~�@bM�@`�9@`A�@`b@`  @_�;@_�@_|�@_+@^�+@^{@]��@]��@]�h@]p�@]`B@]O�@]?}@]�@\��@\��@\(�@[�@[C�@[o@Z�!@Y��@Y��@Y�7@Y&�@X�@X  @W��@W�@Vȴ@VV@U?}@Tj@S�
@S��@S"�@R�\@R�@Q��@Q�^@Qhs@P�9@PQ�@PA�@Pb@O�@Nȴ@N�+@Nv�@Nv�@NE�@M�T@M�-@M�h@M�@Mp�@M?}@L��@Lj@K�
@K��@Kt�@K"�@J�@J��@J^5@I�@I�#@I�7@IX@I&�@H��@H��@Hr�@HA�@G�;@Gl�@G+@G�@F�R@FE�@F{@E�@E�T@E@E`B@E/@D��@D�/@D��@DZ@C�
@CC�@B=q@B=q@B-@BJ@AX@A&�@A%@@��@@Q�@@1'@@b@?�@?��@?�P@?l�@?;d@>ȴ@>ff@>$�@=�T@=`B@<��@<�j@<�D@<z�@<Z@;�m@;�F@;��@;t�@;dZ@;S�@:�\@:J@9�#@9�^@9��@9G�@9%@8�9@8�u@8Q�@81'@8 �@8  @7|�@7�@7
=@6��@6��@6�y@6�@6�R@6��@6��@6v�@6ff@6E�@6{@6{@6@5��@5�h@5`B@4��@4�j@4�@4��@4�D@4I�@3�F@3t�@2�@2�!@2n�@2M�@2M�@2=q@2=q@2-@2-@1�@1�^@1��@1G�@1%@0��@0�`@0Ĝ@0�u@0Q�@/�w@/\)@/
=@.��@.V@.E�@.5?@.5?@.{@-@-�@-`B@-?}@,��@,�D@,�@+ƨ@+S�@+@*�H@*��@*�\@*^5@*=q@)�#@)��@)G�@(��@(�u@(Q�@'��@'+@'
=@'
=@'
=@&�@&��@&v�@&ff@&V@&@%��@%�-@%��@%`B@%�@%V@$�D@$I�@$(�@#�m@#��@#t�@#S�@#33@"��@"~�@!�#@!�7@!hs@!X@!X@!7L@ ��@ Ĝ@ bN@ 1'@�w@��@|�@\)@��@�@��@v�@5?@$�@{@�T@@��@O�@�@�@�@�m@�
@��@��@��@t�@t�@S�@"�@��@�!@^5@=q@-@�#@x�@G�@G�@7L@&�@�9@bN@A�@  @�w@�@�P@\)@;d@
=@�@ȴ@��@ff@{@��@�-@�-@��@�h@p�@O�@?}@?}@/@V@�@�@z�@9X@1@��@�m@�
@ƨ@ƨ@�F@��@C�@��@^5@-@�#@�^@�7@7L@��@�`@Ĝ@bN@1'@b@�@��@��@l�@+@�y@V@�@�-@p�@�@�@z�@(�@(�@1@1@1@1@��@�
@ƨ@��@�@�@t�@S�@S�@C�@"�@
�H@
�!@
�!@
�!@
�\@
~�@
^5@
^5@
M�@
-@	��@	�@	��@	��@	��@	��@	X@	&�@	�@	%@Ĝ@bN@  @�;@��@l�@l�@K�@+@�@
=@�@�R@��@��@��@��@�+@V@$�@@�T@��@��@��@�T@�T@��@�@?}@/@�@�@�D@j@I�@��@��@dZ@33@"�@@�@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��/A��;A��;A��/A��HA��HA��HA��;A��;A��;A��TA��TA��`A��`A��mA��yA��A��mA��`A��mA��`A���A���A��wA��A�+A��HA���A���A�M�A���A�p�A�{A��mA���A�  A�1A��A��!A�~�A���A�n�A���A���A���A��TA�ZA���A�z�A��;A�(�A�
=A��A��A��/A���A���A�\)A�bA���A���A�t�A���A���A��yA�
=A�bA��A�1A��#A��A�?}A�A�Q�A���A��9A�\)A�  A���A�A�A��A��wA�1A���A��A~��A~^5A~Q�Az�/Aw�7Av�Au��Au"�At�RAt(�As�hAsK�Ar��Ar(�AqhsAo�Al�RAk��Ak/Aj�RAi��Ai;dAi�AhjAf�!Ae�hAe�Ad�Ac��Abr�Ab$�AaXA`JA_oA^�!A]dZA\��A[�hAY�TAX�9AX1'AW�-AW+AV1'AU`BAS�ARE�AP=qAN�AL��AL��ALbAJ�9AIXAH��AH�DAH5?AG�AF�AFZAE��AEG�AEoAD�AD��ADr�AD-AD1'AD1'AD �AD �AD �AC�AC|�AC%ABM�A?�#A=�TA;�wA;
=A:{A8��A8JA7��A6��A5dZA4=qA3��A3C�A2�A2�A2�A1A0��A/XA-�^A-\)A,��A,��A,JA*��A)�PA)K�A)&�A(��A'\)A&$�A$�yA$�DA#hsA!hsA �A+A��A�!A  A~�A�A�A�TA��A33A��A�^A+A��A�`AE�A�-A$�A�!AbNA  A�;A�A�AJAt�AA
r�A
 �A	�wA�DA�TA�`A=qAVA �A��A;dAjA��A �!A ZA 5?@�ƨ@�l�@���@���@�x�@� �@�@��`@��@�@���@� �@�;d@�R@�G�@�F@��/@�R@�@�z�@�~�@��@�b@�=q@ߥ�@�~�@��@��m@�X@�9X@�;d@�V@�J@թ�@�V@Դ9@�Q�@� �@Ӯ@Ӆ@�K�@���@�-@Ь@ϥ�@���@˕�@ʇ+@�V@ǍP@�-@��@��@�V@�1@���@��9@��m@��H@�M�@��@���@��@�O�@�&�@�%@��9@�ƨ@�K�@��@��@�
=@���@���@��\@��^@�|�@��!@��\@��@��-@�x�@�hs@���@��@�Z@��m@���@�5?@�z�@�dZ@��R@���@�X@��@�9X@���@��\@�X@�j@��@�S�@�+@��@�n�@�1@�-@��#@���@�x�@�O�@�V@��`@��@�I�@�Q�@�bN@� �@�  @�t�@�|�@�33@���@��H@�ȴ@���@��\@�-@��7@���@�o@���@��@��y@��@��R@�=q@���@�`B@�z�@�A�@� �@�b@�  @���@���@���@��P@��P@��P@�\)@���@�5?@�-@��@�J@��@���@���@�z�@�Z@��m@��@�+@�@���@��@�33@�+@�o@��@�~�@�^5@��@��#@�hs@�&�@��`@�I�@��w@�+@��@��@�o@�@��H@���@���@�v�@�M�@�-@�$�@��@�J@��@��^@���@�`B@��@��u@�1'@��w@�C�@��y@��!@�J@���@��j@�r�@�I�@�9X@� �@�@�P@\)@K�@~�y@~$�@|�/@|��@|j@{�
@{�@{C�@z��@y��@yx�@yG�@xĜ@xA�@x  @w\)@w;d@w|�@w\)@vV@u`B@t��@t�j@s�m@r�H@rM�@q��@q��@q&�@p�`@pr�@o��@o�P@o|�@o;d@n�@n��@nff@m��@mO�@m�@l�/@l�j@l�@l��@lz�@lj@lj@lj@lj@lZ@l(�@l1@k�
@kt�@kdZ@kdZ@kS�@ko@j��@j�\@j^5@j-@i�@iX@hQ�@g�@g�P@gl�@g;d@f�@f�+@f@e/@d��@d�@d�/@d�@dI�@c��@cƨ@c��@c�@ct�@cS�@c"�@b�H@b��@b~�@bM�@`�9@`A�@`b@`  @_�;@_�@_|�@_+@^�+@^{@]��@]��@]�h@]p�@]`B@]O�@]?}@]�@\��@\��@\(�@[�@[C�@[o@Z�!@Y��@Y��@Y�7@Y&�@X�@X  @W��@W�@Vȴ@VV@U?}@Tj@S�
@S��@S"�@R�\@R�@Q��@Q�^@Qhs@P�9@PQ�@PA�@Pb@O�@Nȴ@N�+@Nv�@Nv�@NE�@M�T@M�-@M�h@M�@Mp�@M?}@L��@Lj@K�
@K��@Kt�@K"�@J�@J��@J^5@I�@I�#@I�7@IX@I&�@H��@H��@Hr�@HA�@G�;@Gl�@G+@G�@F�R@FE�@F{@E�@E�T@E@E`B@E/@D��@D�/@D��@DZ@C�
@CC�@B=q@B=q@B-@BJ@AX@A&�@A%@@��@@Q�@@1'@@b@?�@?��@?�P@?l�@?;d@>ȴ@>ff@>$�@=�T@=`B@<��@<�j@<�D@<z�@<Z@;�m@;�F@;��@;t�@;dZ@;S�@:�\@:J@9�#@9�^@9��@9G�@9%@8�9@8�u@8Q�@81'@8 �@8  @7|�@7�@7
=@6��@6��@6�y@6�@6�R@6��@6��@6v�@6ff@6E�@6{@6{@6@5��@5�h@5`B@4��@4�j@4�@4��@4�D@4I�@3�F@3t�@2�@2�!@2n�@2M�@2M�@2=q@2=q@2-@2-@1�@1�^@1��@1G�@1%@0��@0�`@0Ĝ@0�u@0Q�@/�w@/\)@/
=@.��@.V@.E�@.5?@.5?@.{@-@-�@-`B@-?}@,��@,�D@,�@+ƨ@+S�@+@*�H@*��@*�\@*^5@*=q@)�#@)��@)G�@(��@(�u@(Q�@'��@'+@'
=@'
=@'
=@&�@&��@&v�@&ff@&V@&@%��@%�-@%��@%`B@%�@%V@$�D@$I�@$(�@#�m@#��@#t�@#S�@#33@"��@"~�@!�#@!�7@!hs@!X@!X@!7L@ ��@ Ĝ@ bN@ 1'@�w@��@|�@\)@��@�@��@v�@5?@$�@{@�T@@��@O�@�@�@�@�m@�
@��@��@��@t�@t�@S�@"�@��@�!@^5@=q@-@�#@x�@G�@G�@7L@&�@�9@bN@A�@  @�w@�@�P@\)@;d@
=@�@ȴ@��@ff@{@��@�-@�-@��@�h@p�@O�@?}@?}@/@V@�@�@z�@9X@1@��@�m@�
@ƨ@ƨ@�F@��@C�@��@^5@-@�#@�^@�7@7L@��@�`@Ĝ@bN@1'@b@�@��@��@l�@+@�y@V@�@�-@p�@�@�@z�@(�@(�@1@1@1@1@��@�
@ƨ@��@�@�@t�@S�@S�@C�@"�@
�H@
�!@
�!@
�!@
�\@
~�@
^5@
^5@
M�@
-@	��@	�@	��@	��@	��@	��@	X@	&�@	�@	%@Ĝ@bN@  @�;@��@l�@l�@K�@+@�@
=@�@�R@��@��@��@��@�+@V@$�@@�T@��@��@��@�T@�T@��@�@?}@/@�@�@�D@j@I�@��@��@dZ@33@"�@@�@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BĜBĜBÖBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBÖBÖBB��B��B�wB�XB�!B��Bz�BiyB�B�DB�bB�bB�DB�%B�B~�B�B{�Bl�BVBbNB^5BW
BI�B?}B/B�B��B��B�B�/B�/B��BB�'B��B�dB�jB�RB�'B��B��B~�BcTBo�B]/BA�B49B!�BJB
��B
�sB
�;B
�dB
�B
��B
�wB
�jB
ÖB
�RB
�!B
�B
��B
��B
��B
�B
|�B
q�B
jB
e`B
YB
'�B
B
33B
0!B
0!B
1'B
)�B
$�B
%�B
�B
DB	��B	�ZB	��B	�NB	�B	�mB	�/B	�BB	�BB	��B	�FB	�}B	ÖB	ŢB	�jB	�!B	�^B	�B	��B	��B	��B	��B	�bB	�hB	{�B	�B	�+B	�B	~�B	p�B	hsB	P�B	>wB	5?B	$�B	.B	>wB	49B	"�B	�B	'�B	,B	'�B	�B	�B	�B	�B	�B	 �B	 �B	�B	�B	�B	"�B	!�B	�B	�B	�B	oB	%B��B�yBÖBÖBB�
B�BÖB��B�BƨB��B�wBɺB��B��B��BǮBÖB�^B�B��B�FB�?B�'B�B��B��B��B��B��B�JB�DB�7B�=B� Bk�Bz�Bm�BffBhsBhsBaHBYBbNBk�Bk�Be`B`BB]/B_;BbNBaHBXBO�BG�BA�BR�BP�BN�BA�B2-BF�BG�BF�BD�BD�B@�B49B9XB33B2-B-B,B2-B2-B-B(�B)�B2-B33B2-B2-B1'B.B&�B#�B�B"�B&�B�B �B%�B"�B"�B�B�B{B�B�B�B{B�B�B�B\B�B�BoBbB�B �B �B&�B%�B$�B&�B&�B'�B&�B'�B%�B#�B�B�B�B�BuB�B�B�B�B"�B�B!�B+B-B&�B5?B6FB;dBA�BA�BB�BC�BD�BC�BA�B=qB>wB:^B6FB:^BC�BE�BC�B<jB7LBG�BL�BJ�BM�BM�BN�BK�BM�BN�BL�BK�BF�BE�BR�BW
BYB]/B^5BaHBaHB_;BcTBhsBn�Br�Bt�Bq�Bn�BffBq�B�7B�DB�PB�VB�VB�\B�\B�uB��B��B�uB�{B�uB��B��B��B��B��B��B��B��B��B�{B��B�B�B�B�B�B��B�B�B�B�RB�dB�qB�wB��B��BÖBǮBɺBɺBǮBŢBȴB��B��B��B��B��B��B��B��B��B��B�
B�)B�HB�TB�fB�mB�mB�sB�sB�B�B�B�B��B��B��B��B	B	
=B	JB	JB	PB	VB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	(�B	,B	0!B	2-B	1'B	6FB	7LB	A�B	E�B	F�B	G�B	G�B	H�B	J�B	K�B	J�B	I�B	M�B	ZB	ZB	\)B	aHB	dZB	dZB	gmB	m�B	q�B	r�B	u�B	x�B	y�B	� B	�B	�B	�B	�1B	�JB	�JB	�=B	�JB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�-B	�3B	�9B	�?B	�FB	�?B	�?B	�?B	�FB	�LB	�RB	�jB	�jB	�jB	�jB	�qB	�}B	�}B	��B	��B	�}B	��B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�B	��B	�#B	�5B	�;B	�5B	�5B	�5B	�5B	�5B	�BB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�ZB	�ZB	�TB	�ZB	�ZB	�ZB	�mB	�mB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
+B
1B

=B
	7B

=B
DB
DB
DB

=B
JB
JB
JB
VB
\B
VB
VB
bB
hB
hB
hB
bB
hB
oB
oB
oB
hB
hB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
!�B
�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
&�B
'�B
)�B
)�B
)�B
)�B
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
+B
+B
+B
+B
,B
-B
-B
-B
,B
+B
-B
-B
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
2-B
1'B
2-B
33B
33B
33B
2-B
2-B
2-B
33B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
8RB
9XB
:^B
;dB
<jB
;dB
<jB
<jB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
@�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
A�B
B�B
B�B
A�B
A�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
F�B
G�B
H�B
H�B
G�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
K�B
L�B
K�B
K�B
K�B
K�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
N�B
N�B
O�B
N�B
O�B
O�B
N�B
N�B
O�B
O�B
O�B
O�B
N�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
T�B
T�B
T�B
VB
VB
W
B
XB
XB
XB
XB
XB
XB
XB
W
B
W
B
YB
[#B
ZB
[#B
[#B
\)B
\)B
]/B
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
`BB
`BB
`BB
`BB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
e`B
e`B
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
ffB
gmB
gmB
gmB
ffB
ffB
gmB
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
k�B
jB
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BĜBĶBÖBĜBĜBĜBĶBĜBĜBĶBĜBĶBĶBÖBÖBªB��B��B��B��B��B�BHBmCB�uB��B��B��B�0B�B�%B�B�uB|�BncBX+Bb�B^�BW�BKB@�B0�B;B��B�	B�~B�jB�OBуBĜB�B��B�B��B��B��B��B��B��Be�Bp�B_;BDB6B$B�B
�}B
�B
��B
��B
ٚB
ѝB
�4B
��B
�B
�XB
�B
��B
��B
�nB
��B
��B
~(B
sMB
lB
f�G�O�G�O�B
	B
4B
1'B
0�B
1�B
*�B
%�B
&LB
IB
~B	�VB	�B	ϑB	�TB	�=B	�$B	ބB	��B	��B	� B	��B	��B	�MB	�YB	��B	��B	�B	�/B	�WB	��B	��B	�1B	��B	��B	~B	�tB	��B	��B	�B	rB	i�B	R�B	@�B	7�B	'RB	/�B	>�B	5ZB	$�B	QB	(�B	,qB	(�B	�B	�B	pB	~B	dB	!B	!-B	 'B	 B	B	"�B	!�B	�B	�B	�B	�B	�B��B�B�B�%B�B�B�eBżB՛BּB�B�'B� BʌB�~B�dB�xBȀBāB��B�B�B��B��B��B�B��B�B�_B�fB��B�pB��B��B�)B��BnB|Bo�BhXBi�Bi�BcTB[	BcnBk�BlBfLBaHB^OB_�Bb�Ba�BYBQBI�BCaBS[BQ�BO�BC{B4�BG_BH�BG_BEmBESBAoB5�B:^B4�B3hB.�B-CB2�B2�B.IB*0B+6B2�B3�B2�B2�B1�B.�B'�B$�B!-B#�B'�BB!�B&�B#�B#�B�BBSB�B�B�B�B�B�B�B BkBjBB�BxB!|B!|B'8B&LB%`B'8B'8B(>B'RB($B&2B$ZB�B�B�B�B�B�B�B�B �B$B BB# B+�B./B(�B5�B7B;�BA�BA�BB�BC�BD�BC�BA�B>BB?.B;B7�B;0BC�BE�BC�B=qB8�BHKBMBKDBNBNBOBLdBN"BOBMPBLJBG�BGBS�BW�BY�B]�B^�Ba�Ba�B`BBd@Bi*BoBr�BuBrBoOBh
Br�B�lB�xB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B��B�NB�"B�)B�)B�CB�=B�eB�wB��B��B��B�B��B��B��B��B��B��B��B��B�B�%B�B��B�B�,B�2B�@BуB�hB�&B�hB�[B�YB�CB�bB�nB�fB�B�B�B��B��B��B� B�'B�B�+B�`B�PB	�B	
XB	dB	dB	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	$&B	&2B	)_B	,qB	0�B	2|B	1�B	6�B	8B	A�B	E�B	F�B	G�B	G�B	IB	J�B	K�B	J�B	J#B	N<B	Z7B	ZQB	\]B	a�B	d�B	d�B	g�B	m�B	q�B	r�B	u�B	y	B	z*B	�4B	�B	�mB	��B	��B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�,B	�LB	�"B	�OB	�OB	�AB	�MB	�GB	�MB	�TB	�ZB	�`B	�ZB	�tB	�ZB	�zB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�0B	�B	��B	�B	� B	�:B	�&B	�B	�B	�9B	�9B	�B	�B	�B	�$B	�$B	�SB	�{B	�WB	�jB	�;B	�OB	�OB	�OB	�jB	ޞB	�vB	�B	�B	�ZB	�B	�B	�zB	�tB	�B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�B	�$B	��B	�B	�B	�DB	�B	�(B	�B	�JB
 4B
;B
B
B
 B
 B
'B
GB
B
-B
-B
-B
AB
GB
?B
YB
YB
_B
KB
zB
�B

=B
	lB

rB
^B
xB
^B

rB
dB
~B
~B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#B
!�B
 B
#B
$B
$�B
$�B
$�B
&B
%�B
'B
'B
($B
(
B
(
B
'8B
(
B
*B
)�B
*B
)�B
*B
*B
*B
+B
+6B
+B
+B
+6B
+B
+B
+6B
+B
+B
+6B
,"B
-B
-CB
-)B
,=B
+QB
-CB
-CB
/OB
0;B
1AB
1AB
1'B
1AB
1'B
1AB
1AB
1AB
2aB
1AB
2GB
3MB
3MB
3hB
2GB
2GB
2|B
3�B
5tB
5�B
6zB
7LB
7LB
7fB
7�B
7�B
7fB
8�B
8�B
7fB
7�B
7�B
8�B
9�B
:xB
;�B
<�B
;B
<�B
<�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
@�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
A�B
B�B
B�B
A�B
A�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
F�B
G�B
H�B
H�B
G�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
K�B
L�B
K�B
K�B
LB
LB
N�B
N�B
OB
O�B
O�B
PB
O�B
PB
N�B
N�B
O�B
OB
O�B
O�B
N�B
N�B
O�B
O�B
O�B
PB
OB
O�B
Q B
QB
RB
RB
RB
RB
SB
SB
SB
S�B
S&B
SB
S&B
TB
UB
UB
UB
T�B
U2B
UB
VB
VB
VB
UB
UB
U2B
VB
VB
W?B
XB
XB
X+B
XB
X+B
XEB
X+B
WYB
WYB
YKB
[WB
Z7B
[=B
[WB
\CB
\]B
]/B
\CB
\CB
]IB
]IB
^OB
^jB
^jB
^jB
^jB
^jB
^jB
_VB
`\B
`\B
`vB
`vB
b�B
b�B
cnB
c�B
dtB
dtB
dtB
dZB
cnB
dZB
d�B
dtB
dtB
dZB
dtB
dZB
dtB
d�B
dtB
e�B
ezB
ffB
ezB
ezB
ffB
ffB
ffB
f�B
f�B
f�B
f�B
gmB
gmB
g�B
f�B
g�B
g�B
g�B
f�B
f�B
g�B
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
k�B
k�B
k�B
j�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804150035542018041500355420180415003554201806221240192018062212401920180622124019201804271405412018042714054120180427140541  JA  ARFMdecpA19c                                                                20180411093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180411003522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180411003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180411003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180411003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180411003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180411003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180411003527  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180411003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180411003527  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180411003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180411003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20180411005702                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180411153455  CV  JULD            G�O�G�O�F�ѯ                JM  ARSQJMQC2.0                                                                 20180412000000  CF  TEMP_ADJUSTED_QCC0  C2  G�O�                JM  ARCAJMQC2.0                                                                 20180414153554  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180414153554  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180427050541  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034019  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                