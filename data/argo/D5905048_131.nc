CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-06-21T00:35:43Z creation;2017-06-21T00:35:46Z conversion to V3.1;2019-12-19T08:04:24Z update;     
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
_FillValue                 �  IT   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20170621003543  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_131                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @����� 1   @��q��@3nz�G��dhz���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dcy�Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @-p�@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw=qB��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Ds�D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D��D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DE �DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Da��Da�=Dbz=Db�=Dcs�Dc�=Ddz=Dd�=Dez=De��Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�F�D�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�bA��A���Aک�Aڇ+A�l�A�O�A�
=Aٟ�A�Q�A�ĜA��HA׃A�7LA���A�=qA�^5AГuAϺ^A��HA���A̮A�ƨA�"�AʑhA��;A�XAǕ�A�l�A�XA�C�A�A�n�A��A��;A���A���A�1'A�r�A���A���A���A�I�A�hsA���A�=qA�ffA��FA���A�p�A�7LA��^A� �A��uA�p�A���A�ffA�  A��A�Q�A��A�jA�(�A��mA���A�?}A��\A�bA�A�A��A��A�VA���A���A��/A�  A�x�A�=qA�hsA���A���A��A��\A�?}A�1'A�hsA��9A��7A�Q�A�$�A�ƨA�K�A�+A�/A���A��uA���A�1A�^5A�?}AS�A~9XA|E�AzA�Ax^5Av�`Au�AsC�Ar-Ap�!An�Al  AkC�AjJAh�yAhbAfn�Ae
=Ab��A`�RA_ƨA^�A]�-A[?}AYXAV��AU��AT�`AR�HAP��ANM�ALbAKl�AJ�AI��AF1ADbNABȴAA�A?��A>=qA<M�A:�/A9�TA9��A9�A9�A8�RA6�DA4bA3&�A2-A1�A0�\A.��A,ffA* �A)\)A)"�A(�`A(��A(ffA'�wA'��A&�A&JA%A%�A$jA$JA#;dA"ffA �HAoA��A�^A�PAK�A��A&�A��A�\A�TA&�A%AE�A��A�!A��A�A�TAK�A�AJA�PA5?A��A��A�AM�A��A�^A7LA
M�A	�A	p�A��A��Ap�AA��A��AȴAO�A�FA`B@�{@�O�@��@���@�"�@��@��R@�=q@��w@�@���@�X@�&�@��@�u@�G�@�?}@�  @��m@��y@�=q@��#@�Z@�K�@���@�u@�C�@�Q�@�v�@�h@���@�t�@��@���@�V@� �@�n�@��/@�ƨ@���@�@�Ĝ@��@��T@�j@���@Ȭ@�"�@� �@ˍP@�O�@�~�@Ł@��@���@�@�n�@�$�@��T@���@��@���@��@�;d@���@���@�X@�V@�/@��@��u@��@�dZ@���@���@�X@��u@�|�@�7L@��9@�j@��j@�O�@��m@�j@��@�~�@��@�b@��@�t�@��\@�J@���@��@�
=@�V@�$�@���@���@��^@�M�@�v�@��@�X@�A�@���@��F@�dZ@���@���@�M�@�5?@�-@�{@��@�@��@�?}@��`@���@�b@�ƨ@��P@�
=@�~�@�@��#@���@�@��7@�/@���@���@�j@�b@��F@���@���@�K�@�o@�@��y@���@���@��+@�5?@��@��h@�7L@���@���@�1'@�I�@��@�b@���@���@�;d@���@���@�ff@��@��@��@��#@��@���@��9@��D@�r�@�bN@�9X@�b@��@�ƨ@�ƨ@�l�@�@��y@���@�^5@�$�@���@�@���@�`B@�7L@��@�V@���@��D@�I�@�(�@�  @��m@��;@��w@���@��P@�|�@�\)@�
=@��y@��y@�ȴ@���@�v�@�-@��T@���@�x�@�&�@��/@��@�z�@�9X@�9X@�(�@���@��F@�l�@�C�@�
=@�ȴ@���@��!@��\@�v�@�E�@�J@��-@�hs@�7L@��/@��9@�z�@�A�@�  @��;@��w@���@�\)@�
=@��@��@���@��\@�M�@��@���@���@�hs@�?}@�%@���@��j@���@��@�bN@�A�@�  @��;@��F@���@���@��@��@���@��!@�n�@�E�@�5?@��@�J@���@�?}@���@��`@���@��@��@��F@���@�+@��@���@�n�@�-@��@��#@��^@�p�@�7L@���@���@�A�@�1@��
@��w@���@��P@�dZ@�K�@�;d@�+@�+@�@�ȴ@�~�@�^5@�@���@�&�@�%@��/@��@�Q�@�Q�@�A�@�9X@�A�@�9X@� �@�@|�@~@}��@}��@}O�@}/@|z�@|I�@|(�@|�@{��@{�m@{33@z�H@z��@zM�@z�@y�#@y�7@yhs@yX@x��@x�u@w�w@w|�@w\)@v��@u�h@tz�@t1@sdZ@sC�@s"�@rM�@q�@pA�@o\)@nȴ@n5?@m�T@m@mO�@mV@l�@l9X@k�
@k�F@k�@kC�@j�@j��@i�@i�7@iX@iX@i&�@h��@h�@hbN@hQ�@hb@g�;@g�@g;d@f�@fV@fE�@e�@e�h@ep�@eO�@eV@d�j@dz�@d(�@d1@c�@b�H@b=q@a�7@aX@a&�@`�u@` �@_��@_�P@_+@^ȴ@^{@^@]@]V@\j@\I�@[ƨ@[S�@[@Z^5@Y��@Y�#@Y��@Y�^@Y��@YX@X��@X��@Xr�@X1'@X  @W��@V��@VV@U��@U��@U�@T��@Tj@T�@S�m@S��@S�@S33@S@R��@Rn�@RJ@Q�@Q�#@Q��@QG�@P�`@P �@O�;@O��@O
=@N�R@Nff@N$�@Mp�@M?}@Lz�@LI�@L1@Kƨ@K��@KS�@J�H@Jn�@I�@I�7@I�@Hr�@G�;@Gl�@G;d@G�@F�R@Fff@E?}@D��@DZ@D(�@Cƨ@CS�@B^5@B�@A��@A�7@AG�@A%@@�@@b@?�P@?+@>�@>ff@>{@=��@=�@<�/@<�j@<��@<j@<1@;��@;t�@;S�@;o@:��@:��@:n�@:=q@:J@9��@9�^@9�7@9&�@8��@8Ĝ@8Ĝ@8Ĝ@8�9@8��@8�u@8 �@7l�@7\)@7K�@7;d@6ȴ@6ff@6$�@5�@5��@5��@5�h@5�@5`B@5/@5V@4��@4�D@4j@4I�@3��@3��@3�@3"�@2�H@2�\@1�@1�^@1��@1x�@17L@0��@0��@0Q�@0 �@/�;@/|�@/+@.��@.�R@.V@.$�@-�@-��@-�@-`B@-?}@,�j@,Z@,Z@,Z@,9X@+��@+�@+t�@+dZ@+C�@+33@+33@+o@*�\@*^5@*=q@)��@)�7@)hs@)G�@(�9@(�u@(�u@(�@(�@(r�@(r�@(r�@(r�@(bN@(r�@(bN@(bN@(Q�@(Q�@(Q�@(Q�@(Q�@(Q�@(Q�@(1'@( �@'�;@'K�@&�@&�+@&V@&$�@%V@$9X@#��@#�
@#33@"��@"�\@"n�@"=q@"J@!�#@!�^@!�^@!�7@!%@ Q�@  �@�@�@�P@\)@�@��@��@E�@{@@@@��@`B@V@V@��@�@�@�@�@��@��@��@z�@Z@(�@(�@�@�
@��@dZ@S�@S�@"�@�@��@�\@�@��@��@X@�`@�9@�@A�@1'@ �@��@l�@K�@�@�y@��@V@�@O�@�/@�@z�@�@�
@�F@�@t�@dZ@"�@��@=q@-@��@�#@�#@�^@��@hs@&�@��@�9@�u@�@bN@A�@b@  @�w@|�@K�@;d@+@�@
=@�y@�R@V@$�@�-@�h@�h@�@`B@/@�@�@�D@�D@j@Z@(�@1@1@�
@�
@ƨ@��@S�@@
�H@
��@
��@
��@
��@
�\@
^5@
=q@
�@
J@
J@	��@	��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�bA��A���Aک�Aڇ+A�l�A�O�A�
=Aٟ�A�Q�A�ĜA��HA׃A�7LA���A�=qA�^5AГuAϺ^A��HA���A̮A�ƨA�"�AʑhA��;A�XAǕ�A�l�A�XA�C�A�A�n�A��A��;A���A���A�1'A�r�A���A���A���A�I�A�hsA���A�=qA�ffA��FA���A�p�A�7LA��^A� �A��uA�p�A���A�ffA�  A��A�Q�A��A�jA�(�A��mA���A�?}A��\A�bA�A�A��A��A�VA���A���A��/A�  A�x�A�=qA�hsA���A���A��A��\A�?}A�1'A�hsA��9A��7A�Q�A�$�A�ƨA�K�A�+A�/A���A��uA���A�1A�^5A�?}AS�A~9XA|E�AzA�Ax^5Av�`Au�AsC�Ar-Ap�!An�Al  AkC�AjJAh�yAhbAfn�Ae
=Ab��A`�RA_ƨA^�A]�-A[?}AYXAV��AU��AT�`AR�HAP��ANM�ALbAKl�AJ�AI��AF1ADbNABȴAA�A?��A>=qA<M�A:�/A9�TA9��A9�A9�A8�RA6�DA4bA3&�A2-A1�A0�\A.��A,ffA* �A)\)A)"�A(�`A(��A(ffA'�wA'��A&�A&JA%A%�A$jA$JA#;dA"ffA �HAoA��A�^A�PAK�A��A&�A��A�\A�TA&�A%AE�A��A�!A��A�A�TAK�A�AJA�PA5?A��A��A�AM�A��A�^A7LA
M�A	�A	p�A��A��Ap�AA��A��AȴAO�A�FA`B@�{@�O�@��@���@�"�@��@��R@�=q@��w@�@���@�X@�&�@��@�u@�G�@�?}@�  @��m@��y@�=q@��#@�Z@�K�@���@�u@�C�@�Q�@�v�@�h@���@�t�@��@���@�V@� �@�n�@��/@�ƨ@���@�@�Ĝ@��@��T@�j@���@Ȭ@�"�@� �@ˍP@�O�@�~�@Ł@��@���@�@�n�@�$�@��T@���@��@���@��@�;d@���@���@�X@�V@�/@��@��u@��@�dZ@���@���@�X@��u@�|�@�7L@��9@�j@��j@�O�@��m@�j@��@�~�@��@�b@��@�t�@��\@�J@���@��@�
=@�V@�$�@���@���@��^@�M�@�v�@��@�X@�A�@���@��F@�dZ@���@���@�M�@�5?@�-@�{@��@�@��@�?}@��`@���@�b@�ƨ@��P@�
=@�~�@�@��#@���@�@��7@�/@���@���@�j@�b@��F@���@���@�K�@�o@�@��y@���@���@��+@�5?@��@��h@�7L@���@���@�1'@�I�@��@�b@���@���@�;d@���@���@�ff@��@��@��@��#@��@���@��9@��D@�r�@�bN@�9X@�b@��@�ƨ@�ƨ@�l�@�@��y@���@�^5@�$�@���@�@���@�`B@�7L@��@�V@���@��D@�I�@�(�@�  @��m@��;@��w@���@��P@�|�@�\)@�
=@��y@��y@�ȴ@���@�v�@�-@��T@���@�x�@�&�@��/@��@�z�@�9X@�9X@�(�@���@��F@�l�@�C�@�
=@�ȴ@���@��!@��\@�v�@�E�@�J@��-@�hs@�7L@��/@��9@�z�@�A�@�  @��;@��w@���@�\)@�
=@��@��@���@��\@�M�@��@���@���@�hs@�?}@�%@���@��j@���@��@�bN@�A�@�  @��;@��F@���@���@��@��@���@��!@�n�@�E�@�5?@��@�J@���@�?}@���@��`@���@��@��@��F@���@�+@��@���@�n�@�-@��@��#@��^@�p�@�7L@���@���@�A�@�1@��
@��w@���@��P@�dZ@�K�@�;d@�+@�+@�@�ȴ@�~�@�^5@�@���@�&�@�%@��/@��@�Q�@�Q�@�A�@�9X@�A�@�9X@� �@�@|�@~@}��@}��@}O�@}/@|z�@|I�@|(�@|�@{��@{�m@{33@z�H@z��@zM�@z�@y�#@y�7@yhs@yX@x��@x�u@w�w@w|�@w\)@v��@u�h@tz�@t1@sdZ@sC�@s"�@rM�@q�@pA�@o\)@nȴ@n5?@m�T@m@mO�@mV@l�@l9X@k�
@k�F@k�@kC�@j�@j��@i�@i�7@iX@iX@i&�@h��@h�@hbN@hQ�@hb@g�;@g�@g;d@f�@fV@fE�@e�@e�h@ep�@eO�@eV@d�j@dz�@d(�@d1@c�@b�H@b=q@a�7@aX@a&�@`�u@` �@_��@_�P@_+@^ȴ@^{@^@]@]V@\j@\I�@[ƨ@[S�@[@Z^5@Y��@Y�#@Y��@Y�^@Y��@YX@X��@X��@Xr�@X1'@X  @W��@V��@VV@U��@U��@U�@T��@Tj@T�@S�m@S��@S�@S33@S@R��@Rn�@RJ@Q�@Q�#@Q��@QG�@P�`@P �@O�;@O��@O
=@N�R@Nff@N$�@Mp�@M?}@Lz�@LI�@L1@Kƨ@K��@KS�@J�H@Jn�@I�@I�7@I�@Hr�@G�;@Gl�@G;d@G�@F�R@Fff@E?}@D��@DZ@D(�@Cƨ@CS�@B^5@B�@A��@A�7@AG�@A%@@�@@b@?�P@?+@>�@>ff@>{@=��@=�@<�/@<�j@<��@<j@<1@;��@;t�@;S�@;o@:��@:��@:n�@:=q@:J@9��@9�^@9�7@9&�@8��@8Ĝ@8Ĝ@8Ĝ@8�9@8��@8�u@8 �@7l�@7\)@7K�@7;d@6ȴ@6ff@6$�@5�@5��@5��@5�h@5�@5`B@5/@5V@4��@4�D@4j@4I�@3��@3��@3�@3"�@2�H@2�\@1�@1�^@1��@1x�@17L@0��@0��@0Q�@0 �@/�;@/|�@/+@.��@.�R@.V@.$�@-�@-��@-�@-`B@-?}@,�j@,Z@,Z@,Z@,9X@+��@+�@+t�@+dZ@+C�@+33@+33@+o@*�\@*^5@*=q@)��@)�7@)hs@)G�@(�9@(�u@(�u@(�@(�@(r�@(r�@(r�@(r�@(bN@(r�@(bN@(bN@(Q�@(Q�@(Q�@(Q�@(Q�@(Q�@(Q�@(1'@( �@'�;@'K�@&�@&�+@&V@&$�@%V@$9X@#��@#�
@#33@"��@"�\@"n�@"=q@"J@!�#@!�^@!�^@!�7@!%@ Q�@  �@�@�@�P@\)@�@��@��@E�@{@@@@��@`B@V@V@��@�@�@�@�@��@��@��@z�@Z@(�@(�@�@�
@��@dZ@S�@S�@"�@�@��@�\@�@��@��@X@�`@�9@�@A�@1'@ �@��@l�@K�@�@�y@��@V@�@O�@�/@�@z�@�@�
@�F@�@t�@dZ@"�@��@=q@-@��@�#@�#@�^@��@hs@&�@��@�9@�u@�@bN@A�@b@  @�w@|�@K�@;d@+@�@
=@�y@�R@V@$�@�-@�h@�h@�@`B@/@�@�@�D@�D@j@Z@(�@1@1@�
@�
@ƨ@��@S�@@
�H@
��@
��@
��@
��@
�\@
^5@
=q@
�@
J@
J@	��@	��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
�B
�B
�B
�B
�B
.B
?}B
L�B
n�B
��B
�}B�B�B��B��B��BɺB�B�B	7B6FBI�BW
Bl�Bt�B~�B�+B��B��B�bB��B�!B�RBB��B�/B�)B�B��B�wB�}B�}B�jB�XB�RB�-B�FB�FB��B��B��B�B�B��B�bB�uB�bB�Bm�B\)BG�BB�B:^B/B33B1'B-B�BB�`B�B��B�B�dB�qB��B�dB�3B��B��B�1B�Bs�BZB>wB:^B7LB49B.B$�B�B
��B
�;B
��B
�RB
��B
�=B
x�B
k�B
aHB
YB
E�B
49B
'�B
�B
uB
JB
B	��B	�`B	�;B	�
B	��B	ŢB	�dB	�3B	��B	��B	�\B	�=B	�B	s�B	e`B	ZB	N�B	H�B	B�B	33B	(�B	�B	{B	VB	
=B��B�B�sB�HB�#B�B��B��B��B��B��B��BȴBŢB�}B�qB�jB�^B�RB�3B�B��B��B��B��B�B�B�B�9B�^B�wB��BB��B�wB�dB�jB�wB�B��B��B��B�B�FB�'B�B��B��B��B�B�3B�!B�B�LB��B�B�B�)B�/B�)B�)B�#B�)B�)B�#B�5B�BB�HB�;B�5B�)B�#B��B��B��B��BĜB�'B��B�3B��B�RB�LB�XB�jB�dB�^B�^B�^B�LB�B��B��B�!B�'B�3B�wBŢBƨB��B��B��B��B��B�B�B�B�B��BƨB��B�B�#B�;B�HB�;B�HB�HB�HB�TB�HB�5B�B��B��B�B��B��B�mB��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B	  B��B��B��B��B��B	B	B	  B��B��B��B��B	%B	1B	VB	oB	�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	(�B	49B	<jB	>wB	@�B	C�B	C�B	C�B	D�B	E�B	F�B	H�B	I�B	I�B	I�B	J�B	K�B	L�B	L�B	N�B	P�B	VB	XB	ZB	^5B	cTB	gmB	hsB	hsB	hsB	k�B	m�B	o�B	q�B	r�B	u�B	x�B	x�B	y�B	}�B	�B	�B	�B	�B	�%B	�1B	�=B	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�3B	�3B	�LB	�^B	�jB	�wB	�wB	�wB	�}B	��B	��B	ÖB	ĜB	ŢB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�BB	�HB	�NB	�NB	�TB	�TB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
+B
+B
1B

=B
JB
PB
VB
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
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
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
<jB
<jB
=qB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
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
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
G�B
G�B
G�B
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
K�B
K�B
K�B
L�B
L�B
L�B
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
YB
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
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
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
aHB
aHB
cTB
cTB
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
ffB
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
n�B
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
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
jB
;B
7B
�B
�B
B
 �B
/B
@�B
NVB
p�B
�0B
�B#:B�;B�oB֡BϫB��B�yB��B)B7�BKxBZBm�Bv�B��B��B�EB�jB��B�tB��B�	B�gB֡B��B��B�QB͹B��B�UB�B��B�0B�B��B�XB��B�KB�zB��B��B�/B��B�:B�9B��B��Bp�B^�BIBEmB="B0�B4�B3�B0�B�B	7B�KB��B�B��B��B�wB�{B�<B�B�qB��B��B��Bw�B]/B?.B:�B7�B5�B/�B($B�B
�xB
�B
�4B
�PB
�ZB
��B
z�B
mCB
c�B
[qB
G�B
6`B
*KB
�B
B
VB
�B	��B	�B	��B	ؓB	�VB	ǮB	��B	��B	��B	��B	��B	�JB	��B	v`B	h
B	[�B	PbB	K^B	ESB	6+B	+QB	�B	�B	�B	pB�"B��B�B�TB�IB�_BҽB��B�0B�)B˒B��BˬB�fB��B��B��B�B��B�zB��B��B�fB�XB�eB��B��B��B�tB��B�B�uBÖB�oB��B�B�B�'B�]B�nB�@B��B�OB�RB��B��B��B��B��B�!B�B��B��B�FB�MB��B�#B�B�BݲB��BۦB�B�IB��BޞB�-B�B��B�!B�/BܒBӏB��B�FB�uB�+B��B�DB�TB�3B�	B�B��B��B��B��B�JB�PB��B�B�sB�DB�UB�vB�B��B�tB�B�}B�oB҉B�B��B�?B�B�KB��B�BǔBѝB��BۦB�B�B�BB�B�hB�hB��B�:B߾B�YB��B�9B�B�4B��B�8B�zB�fB�lB�aB�5B��B�CB��B��B��B��B�}B�/B��B��B�3B�hB�B��B��B�"B	 iB�cB��B��B�PB��B	�B	B	UB�^B�B��B��B	�B	�B	�B	B	OB	!�B	�B	�B	CB	WB	�B	OB	/B	!B	�B	 B	!�B	%�B	(�B	4TB	<�B	?B	A;B	C�B	C�B	C�B	D�B	FB	F�B	H�B	I�B	I�B	I�B	J�B	LB	MB	MB	O(B	QNB	V9B	XyB	Z�B	^�B	c�B	g�B	h�B	h�B	h�B	k�B	m�B	pB	q�B	sB	u�B	x�B	y	B	zB	~BB	� B	�AB	�-B	�gB	�YB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�FB	�>B	�KB	�WB	�CB	�iB	�[B	�3B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�B	�B	�.B	� B	�B	�B	�,B	�,B	�2B	�9B	�?B	�+B	�+B	�1B	�KB	�KB	�7B	�7B	�WB	�=B	�WB	�]B	�IB	�dB	�dB	�~B	ބB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�	B	�B	�B	��B	��B	�6B	�B	�B	�"B	�]B	�B	�HB
 4B
 4B
 B
 B
AB
AB
GB
GB
3B
9B
EB
zB
�B

�B
~B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
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
B
5B
�B
�B
�B
�B
B
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
"B
!�B
"B
"�B
"�B
# B
#:B
$@B
%,B
%B
&B
&B
&LB
'RB
'RB
(XB
)*B
)*B
*B
*B
*0B
+6B
+QB
+6B
,"B
,=B
,"B
,"B
,=B
-CB
-]B
-CB
-)B
./B
./B
.IB
./B
.IB
.IB
./B
/OB
/OB
/iB
/OB
0UB
0;B
0UB
0UB
1[B
1[B
1AB
1[B
2GB
2GB
2aB
2|B
3�B
3�B
4�B
5ZB
5ZB
5tB
5tB
6zB
6zB
6zB
6�B
6zB
6`B
6zB
6�B
7�B
7fB
7�B
8�B
8�B
8�B
8�B
8lB
8RB
8lB
8lB
8�B
8�B
8�B
8lB
8lB
8lB
8�B
8�B
9�B
9�B
9�B
9�B
9�B
:�B
;B
<�B
<�B
=�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
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
F�B
F�B
F�B
F�B
F�B
GB
G�B
H�B
G�B
G�B
G�B
IB
I�B
I�B
I�B
I�B
I�B
J	B
J�B
KB
J�B
K�B
K�B
K�B
L�B
MB
MB
NB
M�B
M�B
NB
N�B
N�B
OB
OB
OB
PB
O�B
O�B
O�B
O�B
Q B
Q B
Q B
Q B
RB
Q�B
RB
RB
RB
R B
R B
R B
R�B
SB
SB
S&B
S&B
T,B
TB
TB
T,B
TB
TB
T,B
T,B
T,B
U2B
U2B
UB
U2B
U2B
UB
VB
VB
VB
V9B
W?B
W?B
W?B
XEB
XEB
XEB
X+B
X+B
Y1B
Y1B
Y1B
Y1B
ZQB
ZQB
Z7B
ZQB
[=B
[WB
[=B
[=B
[WB
[qB
\CB
\CB
\)B
\CB
\]B
]IB
]/B
]IB
]IB
]/B
]IB
]dB
]~B
^OB
^OB
^�B
^jB
_VB
_pB
_�B
_VB
_VB
_;B
`\B
`\B
`BB
`\B
`\B
`BB
`BB
`BB
`\B
`BB
`BB
`\B
`\B
`BB
`BB
`BB
`vB
`\B
`vB
`vB
a�B
a|B
abB
a|B
a�B
c�B
c�B
dtB
d�B
e�B
e`B
e�B
ezB
f�B
f�B
f�B
ffB
f�B
f�B
f�B
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
iyB
i�B
i�B
i�B
i�B
j�B
j�B
j�B
jB
j�B
j�B
j�B
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
n�B
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
v�B
v�B
w�B
w�B
xB
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{B
z�B
z�B
z�B
{B
z�B
|B
{�B
|B
|B
|B
|B
|B
|B
|B
{�B
|B
|B
|B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201706250037472017062500374720170625003747201806221315102018062213151020180622131510201804050716572018040507165720180405071657  JA  ARFMdecpA19c                                                                20170621093509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170621003543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170621003544  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170621003545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170621003545  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170621003545  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170621003545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170621003545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170621003545  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170621003546                      G�O�G�O�G�O�                JA  ARUP                                                                        20170621011655                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170621153500  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20170624153747  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170624153747  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221657  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041510  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                