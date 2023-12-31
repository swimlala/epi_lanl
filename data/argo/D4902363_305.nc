CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-27T21:36:15Z creation;2018-11-27T21:36:20Z conversion to V3.1;2019-12-19T07:27:03Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20181127213615  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              1A   JA  I2_0576_305                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؓ��P� 1   @ؓ��-� @9�)^��d+S&��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B
=B��B
=B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D��D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�9�D�y�DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�@RD�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�hsA�jA�n�A�n�A�l�A�n�A�p�A�n�A�p�A�l�A�n�A�l�A�l�A�n�A�p�A�p�A�p�A�p�A�r�A�v�A�v�A�v�A�l�A�p�A�p�A�|�A�x�A�VA�?}A�;dA�(�A��Aǧ�A�hsA��/A��A��7A��#A�ȴA���A���A�oA�%A��#A�  A�7LA�bNA�I�A�C�A��A��uA�C�A�"�A�bA��;A��A�=qA��7A�v�A�l�A�1'A��/A�S�A��uA���A�ȴA�A���A���A��A�ƨA�%A���A�~�A�5?A��;A�/A���A�-A���A���A�K�A�ȴA�$�A���A�ZA�=qA�7LA��jA��+A�z�A�\)A��A��A�-A��jA��PA�-A���A���A���A��mA�33A��A�A+A~�RA~M�A}�TA}�A{�FAyXAwƨAu��As�mAq�mAqhsAp�`Ao`BAmhsAk��Ak\)Aj�Agp�Af��Af~�Af^5Ae%AdVAc
=A`��A_�hA^�`A^Q�A]�mA]\)A\5?AY�FAX1AV��AVr�AV(�AU|�AT�AS��AR  AP1'AO\)AN$�AMK�AL��AK��AJ�!AJ{AI��AI%AHffAGp�AE��AD�`AD�AC��AB �AAK�A@��A@jA?��A>n�A>(�A=��A<A�A9�A9VA8�!A7�A7�^A7hsA6��A6$�A5&�A4bNA2��A2=qA2�A1�A1�-A17LA0��A/��A-7LA,v�A,1A+XA*��A*9XA)�A)A(�`A(��A(bA%�7A$~�A#�A#|�A#
=A"(�A!\)A �+A $�A�#A�AƨAffAhsAȴA�#AK�A�/A~�A�#A��A��A|�A�Ar�A�mAXA�/A1AA�\AO�A�A;dA
ZA	��A��A�AĜA�wA��A(�A��A�7A�A5?A �@���@�p�@���@�-@���@�ȴ@�G�@���@���@�bN@�Z@��@�\)@�(�@�`B@�1@�@�@�@��@��m@�-@�9@߅@�
=@�-@��@�Z@�\)@�J@�V@�r�@�  @�+@�E�@��@ԃ@�j@җ�@���@�r�@�  @��m@ϥ�@�t�@�M�@�hs@�Z@˶F@�S�@ʧ�@�p�@�j@��;@�dZ@�ȴ@��@�33@�ff@�=q@�$�@��T@�@�x�@�t�@�=q@��9@�33@�M�@�  @���@�=q@�@��u@��;@�S�@��\@��@�G�@�z�@�t�@�J@�O�@�%@� �@�ff@�%@� �@���@�hs@�j@�1@�|�@���@�j@�1@��P@�t�@�|�@�dZ@�|�@�l�@�ȴ@�^5@�{@�@��@�X@�?}@��@���@��D@�Q�@�9X@��;@��F@���@�+@���@��@�x�@�&�@�Ĝ@�bN@�A�@�1@�K�@���@�v�@�=q@���@��P@��@�v�@��@�@�J@��#@��#@���@���@�@���@��7@�O�@��@�z�@�9X@�A�@���@�Ĝ@�r�@�1'@���@�l�@�|�@�C�@��y@�V@�V@�v�@�n�@�5?@��/@��@��@�ƨ@���@���@��@�+@���@��@���@�M�@��@�{@�J@��@��^@�p�@�?}@�7L@�&�@�V@��`@���@�bN@�  @���@�dZ@�K�@�@�@�o@�o@��@���@�5?@��^@�p�@�`B@�7L@�&�@��@��@���@��u@���@��@�b@���@�"�@���@��@���@�@���@���@�@��7@�`B@�/@���@��j@�  @
=@;d@�P@�  @�@l�@+@}�-@{ƨ@z�H@z�\@z=q@zJ@y��@y�@yx�@y&�@x��@x��@x�@x �@w�w@wK�@v��@vE�@v{@u�@u�-@u`B@uV@t��@tZ@t(�@s��@s@rn�@rJ@q�7@q7L@pĜ@p�@pr�@pQ�@p1'@p  @o�@o�P@o\)@o+@n��@nE�@m?}@l(�@kƨ@k�@kdZ@kS�@kS�@ko@j�\@j^5@j-@jJ@i��@i��@iG�@g|�@f�R@f�+@e�@c��@c@bJ@a��@a��@ahs@a�@`�u@_��@_\)@^�@^{@]�-@]��@]O�@\�@\z�@\Z@\9X@\1@[ƨ@[�F@[dZ@Z��@ZJ@XĜ@XbN@X1'@W�;@W\)@V��@V�y@V�@Vȴ@V��@Vv�@V5?@U�@U�T@U�-@Up�@U�@T(�@R�!@R�@Qhs@Q&�@Q%@Q%@P�`@PĜ@P��@PQ�@P �@P �@P �@O��@O��@Ol�@OK�@O;d@O
=@Nȴ@N�+@Nv�@NE�@M�@M�@Mp�@M`B@MO�@MO�@MO�@M?}@L�@Lz�@LZ@LI�@L�@L�@K�F@Kt�@KdZ@K"�@J�@J�!@J�\@JM�@J=q@JJ@I�^@Ix�@Ihs@Ihs@IG�@H��@H��@G�;@G;d@Fȴ@F�+@E�@E�h@E�@E`B@E/@E�@D�/@DZ@D�@D1@C�
@CdZ@CS�@C@B�\@B=q@A��@A�^@A�@@Ĝ@@��@@�u@@Q�@@1'@@  @?��@?�@>ȴ@>{@=@=�h@=?}@<��@;��@:�@:�H@:�!@:~�@:^5@:M�@:-@:J@9�#@9��@97L@8��@8Q�@7�;@7�P@7+@6��@6ȴ@6$�@6@5�@5�@5�@5�@5�@5�@5�T@5��@5?}@4�/@4�j@4�j@4�j@4��@4�@4(�@3�F@3ƨ@3�F@2�@1��@0�`@0�u@0r�@0�@0r�@0Q�@01'@/�@/��@/|�@/l�@/K�@/�@.ȴ@.��@.v�@.{@-�@-V@,Z@,Z@,j@,I�@,9X@,I�@,j@,z�@,j@+"�@*��@*n�@*^5@*-@)�#@)�7@)�7@)x�@)%@(��@( �@'�w@'l�@'\)@';d@'+@&��@&��@&�y@&�@&��@&5?@%p�@%/@$��@$�@$��@$�D@$z�@$I�@$(�@$�@$�@$(�@$�@#��@#�m@#ƨ@#��@#��@#��@#t�@#C�@"�H@"=q@!�^@ ��@ A�@ 1'@ b@   @   @   @   @�@�@�@�@�;@�;@�@�P@|�@l�@;d@�y@ȴ@�R@�R@�R@�R@��@�+@V@E�@$�@@�T@��@��@��@@��@`B@�@V@V@��@�/@��@�j@��@z�@Z@I�@�@1@1@��@�m@�
@ƨ@�@�H@n�@M�@J@�7@&�@%@��@��@�9@1'@b@��@K�@
=@�R@�+@v�@ff@V@$�@@�@�T@@�-@�-@��@�h@�@p�@��@�@j@�m@C�@@�H@�!@^5@��@�^@hs@��@�@1'@1'@ �@ �@b@�;@�;@��@�@l�@��@�y@�@�R@v�@E�@{@{@��@�@V@��@�/@��@�j@�j@��@��@z�@z�@I�@I�@9X@9X@(�@(�@(�@�@1@��@�m@ƨ@�F@��@��@�@t�@S�@S�@C�@C�@C�@o@
�@
��@
�!@
~�@
J@	��@	��@	�7@	hs@	X@	G�@	7L@	�@��@�`@�`@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@Ĝ@Ĝ@��@r�@1'@b@  @  @  @�@�@�;@�w@��@��@�P@�P@|�@|�@l�@\)@�@�@�@
=@��@�R@ff@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�hsA�jA�n�A�n�A�l�A�n�A�p�A�n�A�p�A�l�A�n�A�l�A�l�A�n�A�p�A�p�A�p�A�p�A�r�A�v�A�v�A�v�A�l�A�p�A�p�A�|�A�x�A�VA�?}A�;dA�(�A��Aǧ�A�hsA��/A��A��7A��#A�ȴA���A���A�oA�%A��#A�  A�7LA�bNA�I�A�C�A��A��uA�C�A�"�A�bA��;A��A�=qA��7A�v�A�l�A�1'A��/A�S�A��uA���A�ȴA�A���A���A��A�ƨA�%A���A�~�A�5?A��;A�/A���A�-A���A���A�K�A�ȴA�$�A���A�ZA�=qA�7LA��jA��+A�z�A�\)A��A��A�-A��jA��PA�-A���A���A���A��mA�33A��A�A+A~�RA~M�A}�TA}�A{�FAyXAwƨAu��As�mAq�mAqhsAp�`Ao`BAmhsAk��Ak\)Aj�Agp�Af��Af~�Af^5Ae%AdVAc
=A`��A_�hA^�`A^Q�A]�mA]\)A\5?AY�FAX1AV��AVr�AV(�AU|�AT�AS��AR  AP1'AO\)AN$�AMK�AL��AK��AJ�!AJ{AI��AI%AHffAGp�AE��AD�`AD�AC��AB �AAK�A@��A@jA?��A>n�A>(�A=��A<A�A9�A9VA8�!A7�A7�^A7hsA6��A6$�A5&�A4bNA2��A2=qA2�A1�A1�-A17LA0��A/��A-7LA,v�A,1A+XA*��A*9XA)�A)A(�`A(��A(bA%�7A$~�A#�A#|�A#
=A"(�A!\)A �+A $�A�#A�AƨAffAhsAȴA�#AK�A�/A~�A�#A��A��A|�A�Ar�A�mAXA�/A1AA�\AO�A�A;dA
ZA	��A��A�AĜA�wA��A(�A��A�7A�A5?A �@���@�p�@���@�-@���@�ȴ@�G�@���@���@�bN@�Z@��@�\)@�(�@�`B@�1@�@�@�@��@��m@�-@�9@߅@�
=@�-@��@�Z@�\)@�J@�V@�r�@�  @�+@�E�@��@ԃ@�j@җ�@���@�r�@�  @��m@ϥ�@�t�@�M�@�hs@�Z@˶F@�S�@ʧ�@�p�@�j@��;@�dZ@�ȴ@��@�33@�ff@�=q@�$�@��T@�@�x�@�t�@�=q@��9@�33@�M�@�  @���@�=q@�@��u@��;@�S�@��\@��@�G�@�z�@�t�@�J@�O�@�%@� �@�ff@�%@� �@���@�hs@�j@�1@�|�@���@�j@�1@��P@�t�@�|�@�dZ@�|�@�l�@�ȴ@�^5@�{@�@��@�X@�?}@��@���@��D@�Q�@�9X@��;@��F@���@�+@���@��@�x�@�&�@�Ĝ@�bN@�A�@�1@�K�@���@�v�@�=q@���@��P@��@�v�@��@�@�J@��#@��#@���@���@�@���@��7@�O�@��@�z�@�9X@�A�@���@�Ĝ@�r�@�1'@���@�l�@�|�@�C�@��y@�V@�V@�v�@�n�@�5?@��/@��@��@�ƨ@���@���@��@�+@���@��@���@�M�@��@�{@�J@��@��^@�p�@�?}@�7L@�&�@�V@��`@���@�bN@�  @���@�dZ@�K�@�@�@�o@�o@��@���@�5?@��^@�p�@�`B@�7L@�&�@��@��@���@��u@���@��@�b@���@�"�@���@��@���@�@���@���@�@��7@�`B@�/@���@��j@�  @
=@;d@�P@�  @�@l�@+@}�-@{ƨ@z�H@z�\@z=q@zJ@y��@y�@yx�@y&�@x��@x��@x�@x �@w�w@wK�@v��@vE�@v{@u�@u�-@u`B@uV@t��@tZ@t(�@s��@s@rn�@rJ@q�7@q7L@pĜ@p�@pr�@pQ�@p1'@p  @o�@o�P@o\)@o+@n��@nE�@m?}@l(�@kƨ@k�@kdZ@kS�@kS�@ko@j�\@j^5@j-@jJ@i��@i��@iG�@g|�@f�R@f�+@e�@c��@c@bJ@a��@a��@ahs@a�@`�u@_��@_\)@^�@^{@]�-@]��@]O�@\�@\z�@\Z@\9X@\1@[ƨ@[�F@[dZ@Z��@ZJ@XĜ@XbN@X1'@W�;@W\)@V��@V�y@V�@Vȴ@V��@Vv�@V5?@U�@U�T@U�-@Up�@U�@T(�@R�!@R�@Qhs@Q&�@Q%@Q%@P�`@PĜ@P��@PQ�@P �@P �@P �@O��@O��@Ol�@OK�@O;d@O
=@Nȴ@N�+@Nv�@NE�@M�@M�@Mp�@M`B@MO�@MO�@MO�@M?}@L�@Lz�@LZ@LI�@L�@L�@K�F@Kt�@KdZ@K"�@J�@J�!@J�\@JM�@J=q@JJ@I�^@Ix�@Ihs@Ihs@IG�@H��@H��@G�;@G;d@Fȴ@F�+@E�@E�h@E�@E`B@E/@E�@D�/@DZ@D�@D1@C�
@CdZ@CS�@C@B�\@B=q@A��@A�^@A�@@Ĝ@@��@@�u@@Q�@@1'@@  @?��@?�@>ȴ@>{@=@=�h@=?}@<��@;��@:�@:�H@:�!@:~�@:^5@:M�@:-@:J@9�#@9��@97L@8��@8Q�@7�;@7�P@7+@6��@6ȴ@6$�@6@5�@5�@5�@5�@5�@5�@5�T@5��@5?}@4�/@4�j@4�j@4�j@4��@4�@4(�@3�F@3ƨ@3�F@2�@1��@0�`@0�u@0r�@0�@0r�@0Q�@01'@/�@/��@/|�@/l�@/K�@/�@.ȴ@.��@.v�@.{@-�@-V@,Z@,Z@,j@,I�@,9X@,I�@,j@,z�@,j@+"�@*��@*n�@*^5@*-@)�#@)�7@)�7@)x�@)%@(��@( �@'�w@'l�@'\)@';d@'+@&��@&��@&�y@&�@&��@&5?@%p�@%/@$��@$�@$��@$�D@$z�@$I�@$(�@$�@$�@$(�@$�@#��@#�m@#ƨ@#��@#��@#��@#t�@#C�@"�H@"=q@!�^@ ��@ A�@ 1'@ b@   @   @   @   @�@�@�@�@�;@�;@�@�P@|�@l�@;d@�y@ȴ@�R@�R@�R@�R@��@�+@V@E�@$�@@�T@��@��@��@@��@`B@�@V@V@��@�/@��@�j@��@z�@Z@I�@�@1@1@��@�m@�
@ƨ@�@�H@n�@M�@J@�7@&�@%@��@��@�9@1'@b@��@K�@
=@�R@�+@v�@ff@V@$�@@�@�T@@�-@�-@��@�h@�@p�@��@�@j@�m@C�@@�H@�!@^5@��@�^@hs@��@�@1'@1'@ �@ �@b@�;@�;@��@�@l�@��@�y@�@�R@v�@E�@{@{@��@�@V@��@�/@��@�j@�j@��@��@z�@z�@I�@I�@9X@9X@(�@(�@(�@�@1@��@�m@ƨ@�F@��@��@�@t�@S�@S�@C�@C�@C�@o@
�@
��@
�!@
~�@
J@	��@	��@	�7@	hs@	X@	G�@	7L@	�@��@�`@�`@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@Ĝ@Ĝ@��@r�@1'@b@  @  @  @�@�@�;@�w@��@��@�P@�P@|�@|�@l�@\)@�@�@�@
=@��@�R@ff@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B?}B>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB?}B?}B?}B?}B?}B?}B?}B>wB>wB>wB?}B@�B@�BB�BE�BW
Be`BffBW
Bk�BiyBH�B9XB2-BC�Bo�BA�B%�BPB&�BI�BR�BR�BE�B@�B?}BB�B;dB!�BhB
=B�BPB��B��BB��B�B��B�qB�B��BÖB�XB�9B�?B��B{�BffB~�B�Bz�Bt�Bk�BcTBcTBYBM�BK�BC�B:^B5?B0!B�BVB�B�B�BoBB
��B
�B
�NB
�}B
�B
�1B
�PB
�B
x�B
q�B
u�B
jB
k�B
l�B
ffB
^5B
M�B
:^B
$�B
�B
{B
DB
B
bB
	7B	��B	�`B	�BB	�B	�5B	ĜB	��B	�/B	�B	ǮB	ÖB	�RB	��B	�B	�!B	�B	��B	��B	�hB	{�B	y�B	q�B	{�B	x�B	p�B	e`B	^5B	R�B	E�B	L�B	B�B	=qB	@�B	9XB	49B	49B	49B	,B	&�B	�B	JB	�B	\B	VB	B	B	B	B��B�B��B�B�5B��B��B�B��B�B��B��B��B�}B��B�dBBȴBŢB��B�jB�3B�B�{B��B�B��B��B��B��B��B��B��B�VBv�Bz�B�B�B�Bw�B{�Bx�B}�By�Bq�BffBdZBaHBffBaHBbNBbNB]/BQ�B?}B@�BT�BQ�BR�BN�BJ�BJ�BB�B=qBA�B7LB8RB1'B49B8RB1'B2-B0!B-B2-B5?B<jB6FB1'B&�B�B#�B#�B"�B7LB2-B'�B.B9XB;dB:^B:^B6FB.B�B�B+B%�B1'B)�B!�B�B�B-B.B2-B.B-B1'B/B,B2-B33B49B2-B0!B0!B49B6FB+B)�B9XB8RB:^B8RB6FB/B1'B0!B5?B6FB33B/B2-B7LB6FB33B,B,B:^BC�BD�BA�B@�B;dB1'B1'B2-B49B;dB;dBG�BT�BT�BO�BXB[#BZB]/B^5B^5B_;B^5BhsBk�BgmBdZBiyBq�Bo�Bt�Bz�B�B� B{�B�%B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�B�9B�?B�LB�^B�dB�XB�jB��B��B�dB�'BŢB��B��B�B�B�
B�B�B�B�B�
B�B�)B�HB�NB�sB�B�B�B�B�B�B�B��B��B��B��B	B	%B	%B	B��B	1B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	"�B	"�B	&�B	)�B	.B	33B	33B	49B	49B	5?B	7LB	8RB	;dB	:^B	?}B	?}B	C�B	E�B	E�B	E�B	D�B	B�B	I�B	P�B	VB	VB	XB	ZB	ZB	YB	YB	\)B	\)B	\)B	]/B	aHB	dZB	jB	r�B	v�B	v�B	w�B	v�B	v�B	w�B	z�B	|�B	}�B	|�B	�B	�JB	�\B	�uB	�oB	�uB	�oB	�PB	�DB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�B	�'B	�'B	�3B	�9B	�9B	�?B	�FB	�XB	�^B	�dB	�jB	�wB	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ÖB	��B	B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	��B	�B	��B	��B	�B	�)B	�BB	�NB	�HB	�HB	�BB	�HB	�TB	�TB	�ZB	�mB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
B
%B
	7B
	7B

=B

=B

=B
	7B
1B
	7B
JB
JB
JB
JB
DB
JB
VB
PB
VB
VB
VB
\B
bB
\B
\B
bB
hB
hB
bB
\B
\B
VB
bB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
"�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
!�B
#�B
#�B
%�B
%�B
&�B
'�B
%�B
(�B
)�B
+B
+B
+B
+B
+B
)�B
)�B
'�B
)�B
+B
,B
-B
-B
-B
,B
,B
.B
.B
,B
+B
.B
1'B
2-B
33B
2-B
2-B
2-B
33B
33B
5?B
5?B
5?B
49B
49B
49B
49B
33B
2-B
49B
49B
9XB
:^B
;dB
<jB
=qB
=qB
=qB
;dB
8RB
=qB
@�B
@�B
@�B
@�B
A�B
B�B
A�B
@�B
@�B
@�B
A�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
C�B
C�B
B�B
E�B
F�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
G�B
F�B
F�B
G�B
E�B
K�B
M�B
M�B
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
O�B
O�B
P�B
P�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
R�B
Q�B
R�B
R�B
S�B
S�B
S�B
R�B
R�B
Q�B
R�B
S�B
T�B
S�B
S�B
S�B
Q�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
T�B
T�B
S�B
R�B
T�B
W
B
VB
T�B
VB
XB
YB
YB
XB
W
B
YB
XB
YB
ZB
ZB
\)B
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
]/B
\)B
\)B
]/B
\)B
\)B
_;B
`BB
`BB
_;B
_;B
bNB
`BB
`BB
bNB
cTB
e`B
e`B
e`B
e`B
dZB
e`B
e`B
dZB
dZB
dZB
e`B
ffB
e`B
e`B
ffB
ffB
gmB
ffB
ffB
e`B
hsB
hsB
iyB
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
iyB
iyB
jB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
jB
jB
jB
jB
jB
jB
iyB
k�B
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
n�B
n�B
n�B
n�B
n�B
m�B
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
p�B
p�B
p�B
r�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B?�B>wB>�B>wB>wB>wB>wB>�B>�B>wB>wB>wB?}B?}B?}B?}B?�B?�B?}B>wB>wB>�B?}B@�B@�BB�BFBW$BezBf�BX+BlWBk�BN�B@B9�BI�Bt�BH�B-�B�B,�BL0BT�BT�BG�BB�BA�BC�B<�B$�B�BdB
B�B�rB�$B B�jB��B�:B�B��B��B�MB��B�%B��B��B��BjB�B�oB{�Bu�BmBd�BdtBZ�BO�BMBD�B;�B6�B1'B/B�ByB�B�B�B%B
�B
�B
�B
��B
�oB
��B
�vB
��B
{0B
s�B
wB
l"B
l�B
m)B
gB
_B
O(B
<�B
'�B
�B
$B
�B
B
 B

=B	��B	�B	�NB	�UB	�B	ǔB	��B	�~B	ؓB	ɆB	��B	�DB	��B	�WB	��B	��B	��B	��B	�@B	~�B	{�B	sMB	|PB	yrB	q�B	f�B	_�B	UB	G�B	M�B	DB	>�B	AoB	:�B	5�B	5B	4�B	-B	'�B	!B	�B	sB	}B	\B	�B	B	�B	�B��B�vB�zB��B�\BөB�,B��B��B�mBԕB��BˬB��B��B�<B�GB�B�B�B�<B�nB��B��B��B��B�B��B�mB�tB�nB��B�+B��By�B|PB��B��B��By$B|�By�B~wBz�Br�BhXBf2Bb�BgmBb�Bc:Bc B^BS[BB�BB�BU�BR�BS�BO�BK�BK�BC�B>�BB[B9$B9�B33B5�B9�B2�B3�B1vB.�B3hB6+B<�B7B2B(sB!|B%�B%zB$�B7�B3B)�B/ B9�B;�B:�B:�B6�B/5B!B~B+�B'8B1�B+B#nB�B 'B-�B/ B2�B.�B-�B1�B/�B-)B2�B3�B4�B2�B1B1'B4�B6�B,qB+B9�B8�B:�B8�B6�B0B1�B0�B5�B6�B3�B0!B2�B7�B6�B3�B-CB-]B:�BC�BD�BA�B@�B<B2�B2-B3MB5tB<jB="BH�BU�BU�BP�BX�B[�BZ�B]�B^�B_B`'B_;BiBlBhXBe�Bj�Br|Bp�Bu�B{�B�uB��B}"B��B��B��B��B��B��B��B�B�CB�B�4B�&B�,B�B�8B�8B�8B�RB�*B�0B�_B�6B�QB�yB��B��B��B��B��B��B��B��B�B��B��B��B�6B��B�B�^B�&B�9B�B�YB�+B�EB�EB�+B�YB�QB�xB�B��B��B�B�B��B��B�B�B��B��B�B�JB�JB	'B	?B	tB	�B	  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#B	#B	'8B	*0B	.IB	3MB	3hB	4TB	4nB	5�B	7�B	8�B	;�B	:�B	?�B	?�B	C�B	E�B	E�B	E�B	D�B	CGB	J	B	Q4B	V9B	VSB	X+B	Z7B	ZQB	YKB	YKB	\CB	\xB	\�B	]�B	a�B	d�B	j�B	r�B	v�B	v�B	w�B	v�B	v�B	xB	{B	}"B	~BB	}�B	�aB	�JB	�BB	�[B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�,B	�2B	�XB	�CB	�OB	�;B	�OB	�OB	�vB	�[B	�hB	�TB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ðB	ðB	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�lB	�4B	�9B	ӏB	�uB	�yB	ܒB	�vB	�hB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�0B	�6B
 4B
;B
B
;B
;B
;B
;B
GB
B
3B
GB
MB
9B
9B
?B
?B
?B
YB
EB
_B
SB
?B
	7B
	lB

XB

XB

XB
	RB
fB
	lB
dB
dB
~B
~B
�B
dB
VB
jB
�B
pB
pB
�B
bB
�B
�B
}B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
�B
�B
�B
�B
�B
�B
B
"�B
#B
#B
#�B
#�B
$B
"�B
"�B
"�B
#B
!�B
#�B
$&B
%�B
&2B
'B
(
B
&2B
(�B
*B
+B
+B
+B
+B
+B
*0B
*B
(>B
*0B
+6B
,"B
-B
-)B
-CB
,=B
,=B
.B
./B
,WB
+�B
.cB
1[B
2aB
3MB
2GB
2GB
2aB
3hB
3MB
5?B
5ZB
5tB
4TB
4TB
4TB
4nB
3�B
2aB
4nB
4�B
9rB
:^B
;�B
<�B
=qB
=�B
=qB
;�B
8�B
=�B
@�B
@�B
@�B
@�B
A�B
B�B
A�B
@�B
@�B
@�B
A�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
C�B
C�B
B�B
E�B
F�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
G�B
F�B
F�B
G�B
FB
K�B
M�B
M�B
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
PB
PB
Q B
P�B
PB
PB
Q B
Q�B
RB
Q�B
Q�B
RB
QB
QB
R�B
RB
SB
SB
S�B
TB
TB
S&B
SB
R B
SB
TB
T�B
S�B
T,B
TB
R B
T,B
TB
UB
UB
U2B
VB
VB
VB
VB
UB
U2B
T,B
S@B
UMB
W$B
VB
U2B
VB
X+B
YB
YKB
X+B
WYB
YKB
XEB
Y1B
Z7B
Z7B
\CB
]IB
]/B
]IB
]dB
]dB
^OB
^5B
^jB
^5B
^OB
^5B
^OB
^jB
]IB
\]B
\]B
]IB
\]B
\xB
_VB
`vB
`vB
_pB
_�B
bNB
`vB
`�B
bhB
c�B
ezB
ezB
e`B
e`B
d�B
ezB
ezB
dtB
d�B
dtB
e`B
ffB
ezB
e�B
f�B
f�B
g�B
f�B
f�B
ezB
h�B
h�B
iyB
iyB
i�B
i�B
i�B
i�B
iyB
i�B
j�B
j�B
jB
jB
j�B
j�B
j�B
iyB
i�B
jB
i�B
jB
j�B
j�B
jB
j�B
jB
k�B
k�B
k�B
jB
j�B
j�B
j�B
j�B
j�B
i�B
k�B
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
n�B
n�B
n�B
n�B
n�B
m�B
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
p�B
p�B
p�B
r�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812020035212018120200352120181202003521201812020200242018120202002420181202020024201812030025492018120300254920181203002549  JA  ARFMdecpA19c                                                                20181128063614  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181127213615  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181127213619  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181127213619  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181127213620  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181127213620  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181127213620  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181127213620  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181127213620  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181127213620                      G�O�G�O�G�O�                JA  ARUP                                                                        20181127215533                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181128153314  CV  JULD            G�O�G�O�Fğ�                JM  ARCAJMQC2.0                                                                 20181201153521  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181201153521  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181201170024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181202152549  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                