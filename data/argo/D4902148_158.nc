CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-12-14T15:37:09Z creation;2018-12-14T15:37:12Z conversion to V3.1;2019-12-18T07:18:17Z update;2022-11-21T05:29:42Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181214153709  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_158                     2C  DdNAVIS_A                         0397                            ARGO 011514                     863 @ؘ#"�� 1   @ؘ#�9 @;�K]�c��d_o� 1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��R@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG�\CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��\A�XA�/A��/A��A���A���A��\A��DA��DA��DA��7A��+A��+A��A��A��A�x�A�x�A�t�A�t�A�t�A�r�A�p�A�dZA�ZA�ZA�\)A�E�A�=qA�1'A�/A�-A�-A�+A�&�A�$�A�$�A�$�A��A��
A�dZA�JA��A�|�A��;A��A��7A���A��;A��A�7LA�C�A��yA��wA�jA��A��PA���A�z�A�%A��7A�`BA��7A��A�S�A��
A�O�A�M�A��A��A�A�hsA���A���A���A�
=A��A�~�A�;dA�1'A~v�A~�A}/A{�^A{7LAzjAy7LAx��AxA�Aw|�Av5?Au�Au%As��Ar��Aq�7Ap=qAn�DAm|�Am7LAl�RAk��Akl�Ak/Ak
=Aj�`Ahz�Af��Ae�7Ad��AdbAb�jA`��A_\)A\ĜAZjAX�RAX  AWXAV�AV��AVr�AU��AU��AUdZAU"�AU
=AT�jATI�AS�AS��AS;dAP  AK��AK��AKp�AK33AJĜAI"�AG��AF�AE`BAD��AB��AB1'AA7LA@1A?�A?�A?%A>z�A=��A=
=A<�jA<ZA:v�A8��A8n�A81'A7�A7��A7O�A6�yA6��A6 �A5�wA5XA4��A4^5A3�A3"�A2~�A1dZA.�!A-|�A,��A,�`A,��A+�A*-A)�hA)�A(�A(9XA'�mA'��A'l�A';dA'�A&��A&ZA%��A%�A#�
A"�!A"VA"1'A"A!��A!�A!7LA �A bNA E�A��A��AO�A�AƨAoA��A�;Ap�AoA�yAZA"�AjAt�A1'A�!A�mAl�A/A�wA	�A�A�!AI�A��A��A�A�DA1'A�A9XA�-A/A��A��A�PA Q�A 1'A (�A  �A J@��m@�K�@���@�V@��7@�l�@���@�M�@���@�@�@�(�@���@��@��/@��@���@���@�  @��@ᙚ@�@�t�@�X@�9X@�\)@ղ-@�+@ҧ�@�7L@�ȴ@͙�@�I�@�t�@�o@�n�@�7L@�9X@�S�@ƸR@��#@��@�1@�@���@�J@�`B@�bN@�ƨ@��@�;d@��!@�-@��/@���@��y@�ff@��T@���@�dZ@��@���@�E�@�{@��@��h@�X@�&�@�z�@�
=@��!@��+@�ff@��#@�&�@���@�j@�  @��P@�o@��R@���@���@��u@�t�@�\)@�+@���@��+@��@���@��@�9X@���@�\)@�"�@��H@�v�@��@���@�?}@�%@���@��9@��@���@�ȴ@���@���@��R@��!@��!@���@��+@�ff@���@�?}@��@��9@��@�1'@��@��F@���@���@���@���@�"�@�7L@��j@��@��D@�9X@���@�;d@���@�V@���@���@��@��u@�(�@�1@��@��w@�;d@�
=@�@�ȴ@���@�@���@���@�G�@�V@�b@��@�\)@�33@�o@�~�@��@���@�O�@�/@��@��@�%@���@��/@��@�bN@��@��@�-@��@���@�Q�@��@��w@���@�C�@��@�o@�
=@���@���@���@���@��`@��`@��/@���@�Ĝ@��j@��9@��u@��@�I�@�1'@��@��@�P@~��@}�@}V@|�j@|j@|I�@|(�@{C�@z�@y�#@y�^@y��@y��@y�7@y�7@yx�@yhs@x�`@x�@xQ�@w�@w�P@v�+@u�h@up�@u/@t��@t��@tZ@r��@rJ@q�^@qX@p��@ol�@n��@nff@nff@nV@nE�@n{@m�@m��@mp�@m?}@mV@l�/@l��@l�@lj@l9X@k��@k�
@k�m@k�m@k�F@kƨ@kS�@j�H@i&�@h�9@hbN@g�w@fȴ@fv�@f5?@e��@ep�@e`B@eV@dI�@c"�@b��@b�!@b�!@b��@bn�@bM�@bJ@a�#@a��@ax�@a7L@`��@`b@_|�@^�@^E�@]�@]�T@]��@]�-@]p�@\��@\��@\Z@[dZ@[33@["�@Z�@Z�!@Z-@Z�@Z�@Y�7@Y&�@X�9@X�@XbN@XA�@W�@W��@W�@W�P@W��@W��@Vȴ@U/@T�@Tj@T9X@T1@S��@S�
@S�
@S�
@S�@St�@St�@SS�@SS�@SC�@S33@So@R�H@R�\@R�@Qx�@P��@P��@P�u@P �@O��@Ol�@O+@N��@Nȴ@N��@Nff@NE�@N5?@N$�@M�@M��@M@M�-@M�-@M�-@M�h@M�@Mp�@MO�@M/@MV@L�j@L9X@K�F@K"�@J�@J��@J�\@J^5@I��@I��@IX@H�`@HbN@F��@F5?@E@Ep�@E`B@EO�@E`B@EO�@EO�@EO�@D�/@C��@B�\@A7L@@�u@@Q�@@ �@?�w@?��@?\)@?+@>��@>ȴ@>��@>V@>@>{@>@=��@=`B@<��@<9X@;�
@;t�@:�@:��@:��@:�!@:��@:~�@:M�@:�@9�#@9�^@9G�@8��@8Ĝ@8�9@8�u@8�u@8�@8�@8A�@8b@7�;@7�w@7�w@7��@7K�@6�@6v�@6$�@5�T@5`B@4(�@3S�@2�H@2��@2��@2��@2��@2n�@1�7@17L@0�9@0r�@0Q�@/��@/\)@/K�@/;d@.�y@.ff@.{@-�@-�h@-p�@-?}@,j@*��@*n�@*�@*�@)��@)��@)��@)G�@)%@(�9@(�u@(�@(r�@(bN@(Q�@(1'@(  @'�@'�@'�;@'�w@'�@'�@'��@'�P@'\)@';d@'+@'�@&�@%�-@#��@#��@#�@#t�@#dZ@#S�@#C�@#33@#o@"�@"�@"�H@"��@"��@"��@"~�@"~�@"^5@"=q@"J@!x�@  �@�@��@E�@5?@{@@�@��@��@@�h@p�@`B@O�@/@��@�j@�D@9X@�
@t�@C�@33@@��@n�@�@�@�^@��@��@�7@��@��@��@��@��@�7@G�@&�@�@��@�`@Ĝ@�9@�9@��@�@�@bN@1'@b@b@��@�@|�@\)@�@��@�y@�@�y@�@�@�@ȴ@�R@ff@5?@E�@E�@V@ff@V@@@�@�T@�T@�T@��@@@�-@��@��@�@`B@O�@/@V@��@�/@�j@�D@9X@��@�
@�F@t�@33@�H@�\@J@�#@��@��@hs@G�@�`@�9@�@Q�@ �@b@  @�;@|�@\)@��@�@ȴ@�R@��@�+@ff@V@�-@O�@/@V@�@1@t�@C�@o@o@o@o@o@@
��@
��@
��@
��@
�H@
�H@
�\@	��@	��@	G�@	7L@	&�@	%@��@�`@�`@��@��@��@�9@�u@�u@�@�@�@�@�@�@r�@bN@A�@  @�@�;@�;@�@�@�@�;@�w@�P@l�@;d@
=@�@ȴ@ȴ@��@��@��@�+@�+@�+@��@��@��@�+@ff@E�@5?@5?@5?@5?@$�@{@�@�T@@p�@`B@O�@O�@/@�D@I�@�@1@�@�@(�@(�@�@��@�F@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��\A�XA�/A��/A��A���A���A��\A��DA��DA��DA��7A��+A��+A��A��A��A�x�A�x�A�t�A�t�A�t�A�r�A�p�A�dZA�ZA�ZA�\)A�E�A�=qA�1'A�/A�-A�-A�+A�&�A�$�A�$�A�$�A��A��
A�dZA�JA��A�|�A��;A��A��7A���A��;A��A�7LA�C�A��yA��wA�jA��A��PA���A�z�A�%A��7A�`BA��7A��A�S�A��
A�O�A�M�A��A��A�A�hsA���A���A���A�
=A��A�~�A�;dA�1'A~v�A~�A}/A{�^A{7LAzjAy7LAx��AxA�Aw|�Av5?Au�Au%As��Ar��Aq�7Ap=qAn�DAm|�Am7LAl�RAk��Akl�Ak/Ak
=Aj�`Ahz�Af��Ae�7Ad��AdbAb�jA`��A_\)A\ĜAZjAX�RAX  AWXAV�AV��AVr�AU��AU��AUdZAU"�AU
=AT�jATI�AS�AS��AS;dAP  AK��AK��AKp�AK33AJĜAI"�AG��AF�AE`BAD��AB��AB1'AA7LA@1A?�A?�A?%A>z�A=��A=
=A<�jA<ZA:v�A8��A8n�A81'A7�A7��A7O�A6�yA6��A6 �A5�wA5XA4��A4^5A3�A3"�A2~�A1dZA.�!A-|�A,��A,�`A,��A+�A*-A)�hA)�A(�A(9XA'�mA'��A'l�A';dA'�A&��A&ZA%��A%�A#�
A"�!A"VA"1'A"A!��A!�A!7LA �A bNA E�A��A��AO�A�AƨAoA��A�;Ap�AoA�yAZA"�AjAt�A1'A�!A�mAl�A/A�wA	�A�A�!AI�A��A��A�A�DA1'A�A9XA�-A/A��A��A�PA Q�A 1'A (�A  �A J@��m@�K�@���@�V@��7@�l�@���@�M�@���@�@�@�(�@���@��@��/@��@���@���@�  @��@ᙚ@�@�t�@�X@�9X@�\)@ղ-@�+@ҧ�@�7L@�ȴ@͙�@�I�@�t�@�o@�n�@�7L@�9X@�S�@ƸR@��#@��@�1@�@���@�J@�`B@�bN@�ƨ@��@�;d@��!@�-@��/@���@��y@�ff@��T@���@�dZ@��@���@�E�@�{@��@��h@�X@�&�@�z�@�
=@��!@��+@�ff@��#@�&�@���@�j@�  @��P@�o@��R@���@���@��u@�t�@�\)@�+@���@��+@��@���@��@�9X@���@�\)@�"�@��H@�v�@��@���@�?}@�%@���@��9@��@���@�ȴ@���@���@��R@��!@��!@���@��+@�ff@���@�?}@��@��9@��@�1'@��@��F@���@���@���@���@�"�@�7L@��j@��@��D@�9X@���@�;d@���@�V@���@���@��@��u@�(�@�1@��@��w@�;d@�
=@�@�ȴ@���@�@���@���@�G�@�V@�b@��@�\)@�33@�o@�~�@��@���@�O�@�/@��@��@�%@���@��/@��@�bN@��@��@�-@��@���@�Q�@��@��w@���@�C�@��@�o@�
=@���@���@���@���@��`@��`@��/@���@�Ĝ@��j@��9@��u@��@�I�@�1'@��@��@�P@~��@}�@}V@|�j@|j@|I�@|(�@{C�@z�@y�#@y�^@y��@y��@y�7@y�7@yx�@yhs@x�`@x�@xQ�@w�@w�P@v�+@u�h@up�@u/@t��@t��@tZ@r��@rJ@q�^@qX@p��@ol�@n��@nff@nff@nV@nE�@n{@m�@m��@mp�@m?}@mV@l�/@l��@l�@lj@l9X@k��@k�
@k�m@k�m@k�F@kƨ@kS�@j�H@i&�@h�9@hbN@g�w@fȴ@fv�@f5?@e��@ep�@e`B@eV@dI�@c"�@b��@b�!@b�!@b��@bn�@bM�@bJ@a�#@a��@ax�@a7L@`��@`b@_|�@^�@^E�@]�@]�T@]��@]�-@]p�@\��@\��@\Z@[dZ@[33@["�@Z�@Z�!@Z-@Z�@Z�@Y�7@Y&�@X�9@X�@XbN@XA�@W�@W��@W�@W�P@W��@W��@Vȴ@U/@T�@Tj@T9X@T1@S��@S�
@S�
@S�
@S�@St�@St�@SS�@SS�@SC�@S33@So@R�H@R�\@R�@Qx�@P��@P��@P�u@P �@O��@Ol�@O+@N��@Nȴ@N��@Nff@NE�@N5?@N$�@M�@M��@M@M�-@M�-@M�-@M�h@M�@Mp�@MO�@M/@MV@L�j@L9X@K�F@K"�@J�@J��@J�\@J^5@I��@I��@IX@H�`@HbN@F��@F5?@E@Ep�@E`B@EO�@E`B@EO�@EO�@EO�@D�/@C��@B�\@A7L@@�u@@Q�@@ �@?�w@?��@?\)@?+@>��@>ȴ@>��@>V@>@>{@>@=��@=`B@<��@<9X@;�
@;t�@:�@:��@:��@:�!@:��@:~�@:M�@:�@9�#@9�^@9G�@8��@8Ĝ@8�9@8�u@8�u@8�@8�@8A�@8b@7�;@7�w@7�w@7��@7K�@6�@6v�@6$�@5�T@5`B@4(�@3S�@2�H@2��@2��@2��@2��@2n�@1�7@17L@0�9@0r�@0Q�@/��@/\)@/K�@/;d@.�y@.ff@.{@-�@-�h@-p�@-?}@,j@*��@*n�@*�@*�@)��@)��@)��@)G�@)%@(�9@(�u@(�@(r�@(bN@(Q�@(1'@(  @'�@'�@'�;@'�w@'�@'�@'��@'�P@'\)@';d@'+@'�@&�@%�-@#��@#��@#�@#t�@#dZ@#S�@#C�@#33@#o@"�@"�@"�H@"��@"��@"��@"~�@"~�@"^5@"=q@"J@!x�@  �@�@��@E�@5?@{@@�@��@��@@�h@p�@`B@O�@/@��@�j@�D@9X@�
@t�@C�@33@@��@n�@�@�@�^@��@��@�7@��@��@��@��@��@�7@G�@&�@�@��@�`@Ĝ@�9@�9@��@�@�@bN@1'@b@b@��@�@|�@\)@�@��@�y@�@�y@�@�@�@ȴ@�R@ff@5?@E�@E�@V@ff@V@@@�@�T@�T@�T@��@@@�-@��@��@�@`B@O�@/@V@��@�/@�j@�D@9X@��@�
@�F@t�@33@�H@�\@J@�#@��@��@hs@G�@�`@�9@�@Q�@ �@b@  @�;@|�@\)@��@�@ȴ@�R@��@�+@ff@V@�-@O�@/@V@�@1@t�@C�@o@o@o@o@o@@
��@
��@
��@
��@
�H@
�H@
�\@	��@	��@	G�@	7L@	&�@	%@��@�`@�`@��@��@��@�9@�u@�u@�@�@�@�@�@�@r�@bN@A�@  @�@�;@�;@�@�@�@�;@�w@�P@l�@;d@
=@�@ȴ@ȴ@��@��@��@�+@�+@�+@��@��@��@�+@ff@E�@5?@5?@5?@5?@$�@{@�@�T@@p�@`B@O�@O�@/@�D@I�@�@1@�@�@(�@(�@�@��@�F@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�mB�5B�!Bl�B1'B�NB��B��B�B��B��B�PB�B~�B{�Bx�Bt�Bl�B_;BP�BM�BD�B@�B2-B�B�BhB	7B
��B
�B
�yB
�ZB
�;B
�B
ǮB
�RB
��B
��B
��B
��B
�VB
�B
~�B
x�B
o�B
jB
e`B
^5B
ZB
VB
O�B
H�B
D�B
>wB
7LB
/B
%�B
�B
oB
DB
1B
B	��B	��B	��B	��B	��B	�mB	�/B	��B	��B	ɺB	��B	�?B	��B	��B	�PB	�B	~�B	|�B	x�B	w�B	v�B	s�B	q�B	p�B	n�B	m�B	l�B	iyB	hsB	e`B	`BB	P�B	=qB	<jB	;dB	:^B	8RB	2-B	.B	(�B	!�B	�B	{B	hB	JB	1B	%B	B	B��B��B��B��B�B�B�TB�BB�;B�5B�)B�B�B�
B��B��B��B��B��BȴBŢB��B�dB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�PB�JB�JB�DB�=B�7B�1B�+B�%B�B�B�B� Bz�Br�BjBdZBcTBbNBaHB_;B\)BYBW
BR�BO�BM�BK�BJ�BF�BC�BB�BA�B@�B@�B?}B>wB=qB;dB9XB7LB6FB49B33B2-B0!B.B.B.B.B.B-B,B,B+B)�B'�B%�B$�B#�B#�B#�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B{BuB{BuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B!�B%�B&�B'�B(�B(�B)�B)�B+B+B+B,B1'B2-B2-B2-B33B5?B6FB7LB8RB9XB:^B;dB=qB=qB@�BC�BD�BD�BD�BE�BG�BG�BI�BL�BN�BO�BO�BP�BQ�BS�BT�BVBW
BVBVBVBcTBp�Bp�Bp�Bq�Bq�Bq�Bq�Br�Bs�Bw�B{�B|�B|�B|�B~�B� B�B�B�B�B�B�B�PB�\B�\B�\B�bB�oB��B��B��B��B��B��B�B�!B�-B�9B�?B�RB�XB�jB��B��B��B��B��B��B��B��B�
B�B�B�B�5B�BB�NB�ZB�`B�fB�fB�mB�mB�sB�B�B�B��B��B	B	B	
=B	PB	VB	bB	�B	�B	�B	�B	�B	�B	 �B	&�B	'�B	'�B	'�B	(�B	(�B	(�B	)�B	+B	,B	.B	/B	/B	1'B	33B	7LB	;dB	=qB	?}B	@�B	A�B	A�B	E�B	J�B	L�B	L�B	M�B	M�B	M�B	M�B	M�B	M�B	O�B	Q�B	R�B	T�B	W
B	[#B	aHB	bNB	cTB	dZB	e`B	ffB	n�B	r�B	t�B	v�B	y�B	~�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�DB	�DB	�DB	�JB	�JB	�PB	�VB	�VB	�VB	�VB	�\B	�VB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�'B	�-B	�-B	�3B	�9B	�?B	�?B	�FB	�RB	�XB	�jB	�qB	�}B	��B	B	B	B	B	ÖB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�;B	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B
	7B
JB
bB
hB
oB
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
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
'�B
)�B
+B
-B
-B
-B
-B
-B
-B
/B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
49B
49B
5?B
5?B
6FB
5?B
7LB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
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
@�B
@�B
?}B
@�B
A�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
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
H�B
J�B
K�B
L�B
M�B
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
N�B
N�B
O�B
O�B
O�B
O�B
P�B
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
ZB
ZB
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
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
iyB
iyB
iyB
iyB
iyB
jB
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
m�B
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
p�B
p�B
p�B
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
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�-B�B�MB�%B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�B�B��B�B��B��B�B��B�B��B��B�B��Bw�B=<B�B��B��B��B�B�_B�(B��B�B|�By�Bv`Bn�Ba|BR BOBE�BB�B5?BOB�B�B^B
��B
�B
�eB
�zB
��B
ؓB
��B
�JB
��B
�|B
��B
��B
�HB
��B
�4B
z^B
poB
k�B
f�B
^�B
[	B
W
B
Q4B
I�B
E�B
?�B
8�B
0�B
'�B
�B
�B
�B
	B
YB	�]B	�<B	�JB	��B	�rB	�B	ޞB	�SB	�B	ˬB	��B	��B	�=B	�IB	�(B	�3B	�B	}�B	y>B	xRB	wfB	tB	rB	qB	n�B	nB	mCB	jB	iDB	gB	dtB	T�B	>B	<�B	<B	;dB	:^B	3�B	/�B	*�B	#B	�B	�B	�B	�B	�B	�B	B	�B	  B��B��B��B�B�"B�B��BߤB��B��BںBؓB��B՛BӏBѝBΥB̘B��B��BÖB�]B��B��B��B��B��B�$B��B��B�|B�\B�5B�B�B�B�B�#B�kB�eB��B��B��B��B��B��B��B��B��B��B��B�zB��B��B��B�B�[B~�Bv`Bl�BeBdBb�BbNB`�B]dBZ�BX�BT�BQ BN�BL�BL�BIBD�BB�BB'BA BA B@iB?cB>BB<�B:xB8B6�B5B3�B3�B1vB.cB.IB.IB.IB.}B-�B,�B,�B+�B+�B)�B'�B%�B$�B%,B$�B$�B#�B#B�B)BB�B�B�B�ByB�BYBSB�B�BB�B�B�BgB9BBBmBSBYB+B?ByB�BEBKB=B=BxB/B�B/B/BjB�BpB �B"hB"�B#B&LB'RB(XB)DB)*B*KB*eB+kB+kB+�B-B1�B2|B2�B2�B3�B5�B6�B7�B8�B9�B:�B<B=�B>]BA;BC�BD�BD�BEBF?BH1BHKBJXBM6BOBBP.BP.BQhBRTBTaBUMBVSBWYBV�BV�BXEBd�Bp�Bp�Bp�Bq�Bq�Bq�Bq�Br�Bt9BxRB|PB}<B}"B}<BHB�4B�;B�;B�;B�UB��B�MB��B��B��B��B��B��B�
B��B�B��B�NB��B�]B�UB�|B�nB��B��B��B��B�'B��B��B�"B�<B�BBбB�aB�YB�_B�eBںBޞB�B�B�B�B�B�B�B�B��B��B��B�cB�zB�jB	�B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	B	kB	!HB	'B	(
B	(
B	(
B	)*B	)*B	)B	*0B	+B	,WB	.IB	/OB	/iB	1[B	3�B	7�B	;�B	=�B	?�B	@�B	A�B	BB	FB	J�B	MB	MB	M�B	NB	NB	M�B	M�B	N"B	P.B	R B	S&B	UMB	W�B	[qB	abB	b�B	c�B	d�B	e�B	gB	n�B	r�B	t�B	wB	zDB	cB	�GB	�B	�MB	�MB	�SB	�?B	�_B	�fB	�RB	�rB	�^B	�xB	�^B	�~B	�dB	��B	�pB	�VB	��B	�pB	��B	��B	��B	�B	��B	��B	�B	�!B	�B	�B	�B	�B	�B	�8B	�XB	��B	�;B	�[B	�AB	�AB	�aB	�aB	�MB	�nB	�ZB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	ªB	ªB	��B	��B	��B	��B	�B	��B	��B	��B	��B	�"B	�B	��B	�.B	� B	�&B	�B	�B	�B	�?B	�$B	�EB	�+B	�1B	�1B	�yB	ڠB	ߊB	�|B	�hB	�B	�TB	�B	�nB	�nB	�B	�tB	�zB	�zB	�`B	�zB	�zB	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�B	�B	�B	�B	�6B	�B	�BB	�(B	�HB
 �B
gB
SB
YB
+B
+B
EB
EB
EB
EB
zB
�B
	�B
�B
�B
�B
�B
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
 B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
#B
#B
#B
#�B
#�B
$B
%B
%B
%�B
'B
'8B
(sB
*eB
+6B
-B
-)B
-B
-CB
-]B
-]B
/OB
0oB
1AB
1[B
1vB
2aB
33B
3MB
3hB
3hB
4nB
4nB
5tB
5ZB
6�B
5�B
7�B
:xB
:�B
;B
;B
;�B
;B
<�B
<�B
=�B
=�B
=qB
>�B
>wB
>�B
>�B
>�B
>�B
>wB
>�B
?�B
?}B
?�B
?}B
?�B
?�B
?�B
@�B
@�B
?�B
A B
BB
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
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
I7B
K)B
K�B
MB
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
OB
N�B
OB
O�B
O�B
PB
PB
Q B
RB
RB
RB
R B
SB
SB
S&B
TB
TB
S�B
S�B
TB
TB
TB
S�B
TB
T,B
TB
U2B
UB
UB
UB
UB
VB
VB
VB
V9B
VB
V9B
VB
V9B
W
B
W?B
W$B
W?B
W?B
W?B
X+B
X+B
XB
X+B
X+B
XB
X+B
X+B
X+B
XEB
YKB
Y1B
Y1B
Y1B
Y1B
YKB
Y1B
Z7B
ZB
Z7B
ZB
ZB
ZB
Z7B
ZB
Z7B
ZB
ZB
ZQB
[=B
[#B
[WB
[WB
[=B
[WB
[=B
[=B
\CB
\CB
\CB
]IB
]IB
]IB
]dB
^OB
^�B
_pB
_pB
_VB
`\B
`\B
`\B
abB
abB
abB
bhB
bNB
b�B
bhB
b�B
c�B
c�B
c�B
dZB
dtB
dZB
d�B
dtB
dtB
d�B
e�B
f�B
f�B
f�B
f�B
g�B
h�B
i�B
i�B
i�B
i�B
i�B
iyB
i�B
j�B
i�B
i�B
i�B
i�B
i�B
j�B
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
m�B
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
p�B
p�B
p�B
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
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9#�<0�|<AT�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812250034022018122500340220181225003402202211182137202022111821372020221118213720201812260020122018122600201220181226002012  JA  ARFMdecpA19c                                                                20181215003623  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181214153709  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181214153711  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181214153711  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181214153711  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181214153711  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181214153712  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181214153712  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181214153712  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181214153712                      G�O�G�O�G�O�                JA  ARUP                                                                        20181214155507                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181214153535  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20181224153402  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181224153402  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181225152012  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171526                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123720  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                