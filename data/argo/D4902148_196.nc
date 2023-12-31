CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-29T15:37:21Z creation;2019-12-29T15:37:24Z conversion to V3.1;2022-11-21T05:27:48Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Id   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  M<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191229153721  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_196                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��#A��1   @��#�-� @<!�X��dy��$�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��R@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C(�C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D ��D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�9�D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D� RD�0R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��+A��uA��\A��PA��DA��7A��A�v�A�jA�p�A�p�A�dZA�O�A�VA�G�A�{A�bA�JA���A���A���A�t�A�S�A� �A�JA���A��A��/A��^A���A���A�p�A�K�A�A�A�;dA�5?A��A�1A���A��TA�r�A�  A�ffA�K�A�ffA��A�|�A�M�A��!A��A��jA���A���A�ZA�VA�1'A���A��A�O�A�Q�A�|�A��A�C�A�O�A��`A���A�r�A�z�A�l�A��A�ZA��#A��\A�(�A�A��!A��HA�33A�oA�
=A�A���A��A�XA�l�A���A�~�A��A���A�~�A�jAO�A}�;A{��Az~�Ay��Ax��AxI�Ax �Aw?}Au�TAu?}At1Ar�+Ar-Ap�Ao�7An-AlĜAk�;AjĜAh9XAf�yAf�\Af  AeoAd�DAc\)AaK�A`�A_\)A^  A]��A]
=A[��AZ1AX��AWp�AV��AU�AT��AT��AT�jAT=qAS��AS�AS��AS�hAR�APffAOXAN�HAN��AN5?AM�wAL�HAI�
AH��AH��AHbNAG��AE�
AD{AB�A@�HA@bA>�/A=��A<��A;�wA;33A:n�A8�`A81'A7��A7�PA6VA57LA4�A4I�A3�A3C�A2-A1��A0��A0�A.�A-�TA-K�A+�mA*��A)��A)?}A(�A(ZA(1A'��A'/A&��A&��A%�#A%`BA%%A$�!A$ZA$�A#��A#oA"^5A!VA �A�hAO�AVA��A^5A��A��AC�A^5A�Ax�A+AJA��AhsA
=Az�A�A��A+A�+AI�Ax�A��Ax�A%A~�A�mA/AZA��A?}A��AE�A�#AƨA�^Al�A�`A��A-AXA
��A
(�A	?}A�uA��A/A~�A�+AĜAK�A �uA r�@���@��R@�^5@�@�p�@�b@��H@���@�r�@�K�@��@��@�  @�@�x�@���@�A�@�33@�7@�ƨ@�E�@� �@���@�-@��@�1'@��;@���@��#@�hs@ܴ9@�S�@�v�@ش9@���@��@��/@�b@�\)@��@�5?@�{@ёh@�/@��@�9X@�@���@�r�@˶F@�;d@��@�M�@���@�G�@�9X@��@�^5@�&�@�A�@Õ�@�33@���@���@�@�-@�x�@��@�@���@�1'@���@�V@�Q�@�@�5?@�X@�;d@��@��@���@��9@���@���@�r�@� �@�ƨ@�t�@���@��\@���@��@�ƨ@�;d@�{@�x�@�V@�1'@�;d@�n�@���@�x�@�p�@�`B@�V@��@�r�@�I�@�b@�;d@�J@�hs@���@�z�@�  @�ƨ@�+@�v�@���@��`@���@�r�@�Q�@�1'@� �@���@��@�o@�$�@�%@��@��!@��+@�$�@���@�7L@�j@�bN@�I�@���@�"�@��\@�M�@�hs@���@�Q�@�1@�
=@��@���@�p�@�O�@���@��@�|�@���@�dZ@�;d@��@���@�5?@���@�x�@�X@�&�@��`@�bN@��@���@��@�r�@��@�l�@���@�n�@�V@�5?@��@�{@���@��-@��@��`@���@�Q�@�bN@�r�@�Q�@�I�@�  @���@�+@���@�ff@�=q@�@��#@�G�@���@�j@�bN@��@��j@�Q�@�P@�@~ȴ@}��@|�/@|��@|I�@{t�@zJ@y��@x��@xb@w��@w;d@w�@v��@v�y@v�y@v�R@v��@w�@vȴ@v@t�/@t��@tz�@tj@s�F@sƨ@s@q�#@qhs@qX@qhs@qhs@qX@q%@p�9@pr�@pb@o|�@o�@n�@n�R@nff@n5?@n@m�-@m`B@l�@l�j@lz�@l1@kS�@ko@jn�@j=q@j�@i�#@iX@i%@h�`@h�9@h  @g��@g
=@fv�@f$�@e�@e@e�h@e?}@d��@dj@dZ@dI�@d9X@c��@c��@cC�@c@c@b�@b�H@b��@b~�@bJ@a�@a��@a��@a7L@`��@`�@`r�@`bN@`Q�@`A�@`  @_�P@_K�@^�y@^��@^ff@^E�@]@]`B@\��@[ƨ@[C�@Z�H@Z��@Z��@Z~�@Z^5@Z=q@Y��@Y��@Y&�@X��@Xr�@X �@W�P@WK�@W+@V�@Vff@Vff@Vff@VV@V{@U�h@U�@UO�@T�@T�D@T9X@S�m@S�@R�H@R��@R�\@R�\@R~�@R�@Qx�@Q7L@P��@Pr�@O��@O+@N�y@Nȴ@N��@N��@N��@N��@N��@N�+@N@M/@L�@L�j@Lz�@L1@K�F@K��@K@Jn�@J-@I�^@I7L@I%@H��@H1'@G�P@G�@FV@F@E�h@E`B@E/@E/@D�/@DZ@D(�@D�@C�m@C�@Co@B��@B��@B�!@B�\@B�@Ahs@A&�@@�`@@��@@r�@@A�@@b@?��@?��@?�@?l�@>�y@>v�@>5?@>{@=`B@=O�@=�@<��@<�j@<�D@<9X@<1@;ƨ@;@:�!@:~�@:=q@:�@:�@9��@9��@9�@9��@9&�@8�9@8�@8A�@7�@7�@7�P@7K�@7�@6�R@6�+@6v�@6V@6$�@6@6@5��@5@5�@5p�@5`B@5?}@4�/@4��@4z�@4(�@3��@3��@3�m@3�m@3�m@3�
@3��@3S�@2~�@2M�@2J@1�#@1hs@1G�@1%@0Ĝ@0��@0�u@01'@/�@/��@/��@/�@/�P@/K�@.�y@.��@.��@.�+@.v�@.$�@-�@-O�@-/@-�@,�@,��@,1@+�
@+��@+"�@+o@+@*�H@*��@*��@*=q@*-@*-@*-@*-@*-@)�#@)�7@)G�@)�@(��@(�`@(��@( �@'�@'�@'\)@&�y@&�@&�R@&�+@&{@%@%�h@%�@%p�@%?}@%V@$��@$�@$�@$��@$z�@$(�@#��@#�
@#�@#C�@#33@#"�@#@"�@"��@"�!@"�!@"��@"M�@"=q@"�@!�^@!x�@!&�@!�@ �`@ �u@ Q�@�;@�P@l�@\)@K�@;d@+@�@�y@$�@�h@V@�j@�@�D@j@(�@1@�m@�F@�@dZ@dZ@�@�!@n�@M�@-@hs@7L@&�@�@%@��@��@�`@�`@��@A�@b@b@b@ �@b@��@�P@l�@+@�y@�R@��@�+@v�@V@E�@�@�T@@�-@p�@O�@V@�@��@�@z�@I�@(�@�m@��@��@t�@dZ@S�@"�@@�@�@�H@��@�\@n�@^5@=q@J@�@�#@�#@�^@�7@hs@G�@�@%@��@Ĝ@�u@r�@bN@1'@�@��@�w@�w@�@��@\)@�y@��@v�@V@V@$�@@��@�h@p�@?}@/@�@��@�j@�D@9X@ƨ@t�@S�@C�@33@o@@
��@
�!@
n�@
=q@
=q@
=q@
-@
J@	�@	��@	X@�`@�@1'@ �@b@�@��@
=@�y@�y@�@ȴ@�R@��@��@ff@E�@{@@@��@��@��@��@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��+A��uA��\A��PA��DA��7A��A�v�A�jA�p�A�p�A�dZA�O�A�VA�G�A�{A�bA�JA���A���A���A�t�A�S�A� �A�JA���A��A��/A��^A���A���A�p�A�K�A�A�A�;dA�5?A��A�1A���A��TA�r�A�  A�ffA�K�A�ffA��A�|�A�M�A��!A��A��jA���A���A�ZA�VA�1'A���A��A�O�A�Q�A�|�A��A�C�A�O�A��`A���A�r�A�z�A�l�A��A�ZA��#A��\A�(�A�A��!A��HA�33A�oA�
=A�A���A��A�XA�l�A���A�~�A��A���A�~�A�jAO�A}�;A{��Az~�Ay��Ax��AxI�Ax �Aw?}Au�TAu?}At1Ar�+Ar-Ap�Ao�7An-AlĜAk�;AjĜAh9XAf�yAf�\Af  AeoAd�DAc\)AaK�A`�A_\)A^  A]��A]
=A[��AZ1AX��AWp�AV��AU�AT��AT��AT�jAT=qAS��AS�AS��AS�hAR�APffAOXAN�HAN��AN5?AM�wAL�HAI�
AH��AH��AHbNAG��AE�
AD{AB�A@�HA@bA>�/A=��A<��A;�wA;33A:n�A8�`A81'A7��A7�PA6VA57LA4�A4I�A3�A3C�A2-A1��A0��A0�A.�A-�TA-K�A+�mA*��A)��A)?}A(�A(ZA(1A'��A'/A&��A&��A%�#A%`BA%%A$�!A$ZA$�A#��A#oA"^5A!VA �A�hAO�AVA��A^5A��A��AC�A^5A�Ax�A+AJA��AhsA
=Az�A�A��A+A�+AI�Ax�A��Ax�A%A~�A�mA/AZA��A?}A��AE�A�#AƨA�^Al�A�`A��A-AXA
��A
(�A	?}A�uA��A/A~�A�+AĜAK�A �uA r�@���@��R@�^5@�@�p�@�b@��H@���@�r�@�K�@��@��@�  @�@�x�@���@�A�@�33@�7@�ƨ@�E�@� �@���@�-@��@�1'@��;@���@��#@�hs@ܴ9@�S�@�v�@ش9@���@��@��/@�b@�\)@��@�5?@�{@ёh@�/@��@�9X@�@���@�r�@˶F@�;d@��@�M�@���@�G�@�9X@��@�^5@�&�@�A�@Õ�@�33@���@���@�@�-@�x�@��@�@���@�1'@���@�V@�Q�@�@�5?@�X@�;d@��@��@���@��9@���@���@�r�@� �@�ƨ@�t�@���@��\@���@��@�ƨ@�;d@�{@�x�@�V@�1'@�;d@�n�@���@�x�@�p�@�`B@�V@��@�r�@�I�@�b@�;d@�J@�hs@���@�z�@�  @�ƨ@�+@�v�@���@��`@���@�r�@�Q�@�1'@� �@���@��@�o@�$�@�%@��@��!@��+@�$�@���@�7L@�j@�bN@�I�@���@�"�@��\@�M�@�hs@���@�Q�@�1@�
=@��@���@�p�@�O�@���@��@�|�@���@�dZ@�;d@��@���@�5?@���@�x�@�X@�&�@��`@�bN@��@���@��@�r�@��@�l�@���@�n�@�V@�5?@��@�{@���@��-@��@��`@���@�Q�@�bN@�r�@�Q�@�I�@�  @���@�+@���@�ff@�=q@�@��#@�G�@���@�j@�bN@��@��j@�Q�@�P@�@~ȴ@}��@|�/@|��@|I�@{t�@zJ@y��@x��@xb@w��@w;d@w�@v��@v�y@v�y@v�R@v��@w�@vȴ@v@t�/@t��@tz�@tj@s�F@sƨ@s@q�#@qhs@qX@qhs@qhs@qX@q%@p�9@pr�@pb@o|�@o�@n�@n�R@nff@n5?@n@m�-@m`B@l�@l�j@lz�@l1@kS�@ko@jn�@j=q@j�@i�#@iX@i%@h�`@h�9@h  @g��@g
=@fv�@f$�@e�@e@e�h@e?}@d��@dj@dZ@dI�@d9X@c��@c��@cC�@c@c@b�@b�H@b��@b~�@bJ@a�@a��@a��@a7L@`��@`�@`r�@`bN@`Q�@`A�@`  @_�P@_K�@^�y@^��@^ff@^E�@]@]`B@\��@[ƨ@[C�@Z�H@Z��@Z��@Z~�@Z^5@Z=q@Y��@Y��@Y&�@X��@Xr�@X �@W�P@WK�@W+@V�@Vff@Vff@Vff@VV@V{@U�h@U�@UO�@T�@T�D@T9X@S�m@S�@R�H@R��@R�\@R�\@R~�@R�@Qx�@Q7L@P��@Pr�@O��@O+@N�y@Nȴ@N��@N��@N��@N��@N��@N�+@N@M/@L�@L�j@Lz�@L1@K�F@K��@K@Jn�@J-@I�^@I7L@I%@H��@H1'@G�P@G�@FV@F@E�h@E`B@E/@E/@D�/@DZ@D(�@D�@C�m@C�@Co@B��@B��@B�!@B�\@B�@Ahs@A&�@@�`@@��@@r�@@A�@@b@?��@?��@?�@?l�@>�y@>v�@>5?@>{@=`B@=O�@=�@<��@<�j@<�D@<9X@<1@;ƨ@;@:�!@:~�@:=q@:�@:�@9��@9��@9�@9��@9&�@8�9@8�@8A�@7�@7�@7�P@7K�@7�@6�R@6�+@6v�@6V@6$�@6@6@5��@5@5�@5p�@5`B@5?}@4�/@4��@4z�@4(�@3��@3��@3�m@3�m@3�m@3�
@3��@3S�@2~�@2M�@2J@1�#@1hs@1G�@1%@0Ĝ@0��@0�u@01'@/�@/��@/��@/�@/�P@/K�@.�y@.��@.��@.�+@.v�@.$�@-�@-O�@-/@-�@,�@,��@,1@+�
@+��@+"�@+o@+@*�H@*��@*��@*=q@*-@*-@*-@*-@*-@)�#@)�7@)G�@)�@(��@(�`@(��@( �@'�@'�@'\)@&�y@&�@&�R@&�+@&{@%@%�h@%�@%p�@%?}@%V@$��@$�@$�@$��@$z�@$(�@#��@#�
@#�@#C�@#33@#"�@#@"�@"��@"�!@"�!@"��@"M�@"=q@"�@!�^@!x�@!&�@!�@ �`@ �u@ Q�@�;@�P@l�@\)@K�@;d@+@�@�y@$�@�h@V@�j@�@�D@j@(�@1@�m@�F@�@dZ@dZ@�@�!@n�@M�@-@hs@7L@&�@�@%@��@��@�`@�`@��@A�@b@b@b@ �@b@��@�P@l�@+@�y@�R@��@�+@v�@V@E�@�@�T@@�-@p�@O�@V@�@��@�@z�@I�@(�@�m@��@��@t�@dZ@S�@"�@@�@�@�H@��@�\@n�@^5@=q@J@�@�#@�#@�^@�7@hs@G�@�@%@��@Ĝ@�u@r�@bN@1'@�@��@�w@�w@�@��@\)@�y@��@v�@V@V@$�@@��@�h@p�@?}@/@�@��@�j@�D@9X@ƨ@t�@S�@C�@33@o@@
��@
�!@
n�@
=q@
=q@
=q@
-@
J@	�@	��@	X@�`@�@1'@ �@b@�@��@
=@�y@�y@�@ȴ@�R@��@��@ff@E�@{@@@��@��@��@��@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�%B�%B�%B�%B�%B�%B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�%B�+B�+B�1B�7B�7B�=B�=B�DB�DB�DB�=B�=B�7B�1B�B�B�%B�B�Bz�BjBYB>wB.B��B�TB�B��BƨB�^B�jB�}BɺB�B�B��B�B�
B��BĜB�FB��B�VB�%B}�Bx�Bs�Bo�BhsBM�BB�B?}B>wB<jB5?B�B
��B
�/B
��B
��B
ƨB
B
�XB
��B
�PB
� B
p�B
gmB
aHB
]/B
W
B
T�B
M�B
C�B
>wB
6FB
-B
(�B
 �B
�B
JB
B	��B	��B	�ZB	�/B	�B	�B	��B	��B	ÖB	�RB	�9B	�B	��B	��B	��B	��B	�PB	�%B	� B	z�B	u�B	r�B	r�B	p�B	m�B	k�B	jB	jB	hsB	cTB	YB	R�B	P�B	N�B	L�B	I�B	D�B	8RB	2-B	1'B	/B	+B	 �B	�B	bB	
=B	B	  B��B��B�B�B�B�`B�HB�BB�/B�B��B��B��B��BɺBŢBÖB��B�dB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�hB�bB�VB�JB�DB�1B�%B�B�B�B�B�B� B}�B|�By�Bx�Bv�Bv�Bt�Br�Bq�Bp�Bo�Bn�Bm�Bl�Bk�BiyBhsBffBdZBcTBaHB`BB_;B]/BZBZBYBXBXBXBW
BVBT�BT�BR�BQ�BO�BM�BL�BJ�BH�BG�BC�B@�B>wB=qB<jB;dB;dB:^B:^B9XB9XB8RB7LB6FB6FB5?B49B33B2-B1'B/B.B,B,B,B+B+B+B+B+B+B)�B(�B)�B)�B(�B(�B(�B(�B)�B(�B+B+B,B,B-B-B-B-B-B-B,B.B.B.B.B.B.B-B-B-B/B2-B33B6FB7LB8RB9XB9XB9XB9XB:^B:^B;dB>wB@�BC�BC�BE�BJ�BM�BN�BO�BS�BYB[#B\)B\)B\)B]/B]/B^5B_;B`BBaHBbNBe`BhsBk�Bl�Bp�Br�Bs�Bv�Bz�B|�B�B�B�B�B�B�B�B�B�B�+B�=B�JB�PB�VB�\B�\B�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�'B�-B�9B�FB�LB�LB�XB�RB�LB�XB�qB��BĜBǮB��B��B��B�
B�/B�;B�5B�;B�NB�ZB�yB�yB�yB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	1B	JB	PB	VB	VB	VB	bB	hB	{B	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	%�B	&�B	(�B	(�B	)�B	-B	1'B	2-B	49B	49B	6FB	8RB	=qB	?}B	?}B	A�B	F�B	K�B	M�B	P�B	Q�B	S�B	VB	W
B	XB	ZB	\)B	]/B	`BB	cTB	e`B	ffB	ffB	gmB	gmB	hsB	k�B	o�B	p�B	r�B	v�B	w�B	w�B	y�B	z�B	}�B	~�B	� B	�B	�B	�%B	�+B	�1B	�7B	�7B	�DB	�PB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�-B	�3B	�9B	�FB	�FB	�LB	�LB	�LB	�LB	�RB	�^B	�^B	�^B	�dB	�jB	�jB	�wB	�wB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
+B
	7B
1B
	7B
	7B

=B

=B
DB
JB
JB
PB
PB
\B
bB
hB
hB
hB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
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
-B
-B
.B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
8RB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
;dB
<jB
<jB
=qB
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
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
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
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
W
B
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
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
`BB
`BB
`BB
`BB
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
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
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
hsB
iyB
iyB
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
l�B
m�B
m�B
n�B
n�B
n�B
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
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�%B�%B�?B�%B�%B�?B�?B�3B�MB�B�B�MB�3B�B�gB��B�3B�MB��B��B�mB�mB��B��B�mB�mB�?B�YB�zB�_B��B��B��B�XB�XB�^B�xB�xB�rB��B��B�#B�B��B��B�RB�_B�Bn�B_!BB�B5�BUB�BؓBөBɆB�6B�wB��B˒B�B�B��B�$B�EB֡B�B��B��B��B�EB~�By�BtnBq'Bk�BO(BB�B?�B>�B>B9XB�B
��B
ޞB
��B
��B
�zB
�MB
�PB
�B
��B
�[B
q�B
h�B
b4B
]�B
W�B
V9B
O\B
D�B
@ B
7�B
-�B
*B
"�B
EB
�B
�B	��B	�fB	��B	��B	�	B	�$B	� B	͟B	żB	��B	��B	��B	��B	�B	��B	�yB	�(B	��B	�;B	|B	vzB	r�B	sB	q[B	nB	k�B	j�B	kB	i�B	e�B	ZQB	S�B	QhB	O�B	M�B	KxB	G�B	9�B	2�B	1�B	0UB	-�B	# B	�B	oB	�B	�B	�B�JB��B�B��B�]B�fB��B�-B޸B�eBԯBуBϫB��B�)BƨBĜB��B�"B��B�TB��B�WB�sB��B�bB�pB�OB�CB�=B�B�1B��B�MB�B��B��B��B�4B�\B��B�B�lB��B��B��B��B��B��B��B~�B~(Bz�ByXBw�BxBuZBs3BrGBqvBp;BoOBncBmwBl"Bj�Bi�Bg�BeBd&Bb4Ba|B`vB^B[	B[	BY�BX�BXEBX_BW�BV�BU�BU�BTBR�BQBOBM�BK�BJ#BIRBF?BB�B@OB>]B<�B<PB<B:�B:�B9�B:xB9XB88B7LB72B6FB5%B4B3B2|B1B/�B-)B-]B-wB,WB,�B+�B+�B+�B+�B*B)�B*�B*B)�B*B)�B*KB*�B*0B+�B+�B,�B,�B-�B-]B-wB-wB-wB-�B-�B.�B.�B.�B.�B.�B.�B-�B-�B.B0B2�B4B6�B7�B8�B9�B9�B9�B9�B;B;0B<�B?�BA�BD3BD�BF�BK�BN�BO�BQNBT�BY�B[qB\]B\]B\]B]dB]~B^�B_�B`�Ba�BcBf2Bi*Bl"Bm]Bq'BsMBt�Bw�B{B}�B�;B� B�UB�oB�[B�MB��B��B��B�B��B��B��B��B��B��B�&B�?B�=B��B�B�B��B��B� B�:B��B��B��B� B��B�UB��B��B��B��B�zB��B��B��B��B��B��B��B�B�B�KB�0B�NBԯB�sBݲB��B�jB�pB�B�B��B��B��B��B��B�B�B��B��B��B��B��B��B�6B��B�qB�HB	;B	[B	SB	KB	dB	�B	�B	�B	�B	�B	�B	{B	�B	�B	�B	�B	/B	!B	#B	%B	&B	&2B	'RB	)DB	)DB	*B	,�B	1AB	2�B	4�B	4�B	6�B	8�B	=�B	?�B	?�B	A�B	GEB	K�B	NVB	Q4B	R:B	TFB	VB	W?B	X+B	ZQB	\CB	]dB	`BB	c�B	e�B	f�B	f�B	g�B	g�B	h�B	k�B	pB	qB	r�B	v�B	w�B	w�B	zB	{B	~B	.B	�OB	�AB	�SB	�?B	�EB	�KB	�RB	�RB	�xB	��B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�0B	�6B	�"B	�CB	�]B	�cB	�AB	�-B	�aB	�GB	�GB	�hB	��B	�`B	�`B	�fB	�fB	�fB	��B	��B	�xB	�xB	�xB	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B	��B	ĶB	żB	��B	��B	��B	��B	�)B	�B	�B	�B	�B	�B	�B	�B	�,B	�B	�9B	�_B	�QB	�kB	�qB	�dB	�jB	�jB	�VB	�\B	�BB	�vB	�\B	�|B	�B	�B	�B	�zB	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�2B	�	B	��B	�B	�B	�B	�B	�"B	�(B
 B
 OB
UB
'B
aB
gB
SB
YB
�B
	RB
�B
	lB
	RB

XB

�B
�B
dB
dB
jB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!B
!B
!�B
"�B
$B
#�B
#�B
#�B
$�B
%B
%B
%B
%B
%�B
'B
'B
'B
'B
'B
'B
'B
(
B
(
B
)B
)B
)B
)�B
*0B
*0B
*B
+B
+6B
+B
+QB
,"B
,"B
,"B
-)B
-B
-B
-B
-B
-CB
-)B
.cB
/iB
0;B
0;B
1[B
1vB
1AB
2GB
2GB
2aB
3hB
3hB
4TB
4nB
4TB
4nB
5ZB
6`B
6`B
7fB
7�B
8lB
7�B
7�B
8�B
8lB
9rB
9rB
9�B
:�B
;�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?}B
?�B
?}B
?}B
?�B
?�B
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
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
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
NB
M�B
M�B
M�B
M�B
N�B
OB
OB
N�B
O(B
PB
Q B
P�B
Q B
P�B
Q B
Q B
RB
R:B
R:B
R B
S&B
R�B
S&B
S&B
SB
T,B
T,B
TB
U2B
U2B
UB
VSB
V9B
VB
W?B
XEB
XEB
XEB
YB
Y1B
YB
Y1B
YB
ZB
Z7B
Z7B
[WB
[=B
\CB
\CB
\CB
\]B
\CB
\]B
\]B
\]B
\]B
\CB
]/B
]dB
]IB
]IB
]dB
]IB
^5B
^OB
^OB
^jB
^jB
_pB
_VB
_pB
_pB
_VB
`\B
`\B
`\B
`\B
abB
abB
aHB
abB
a|B
bNB
bhB
bNB
bNB
b�B
bhB
c�B
cnB
c�B
cnB
cnB
dZB
dtB
d�B
e�B
e�B
ezB
ezB
ezB
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
hsB
h�B
hsB
h�B
h�B
h�B
i�B
i�B
j�B
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
l�B
m�B
m�B
n�B
n�B
n�B
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
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001090034122020010900341220200109003412202211182141292022111821412920221118214129202001100023522020011000235220200110002352  JA  ARFMdecpA19c                                                                20191230003719  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191229153721  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191229153723  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191229153723  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191229153724  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191229153724  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191229153724  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20191229153724  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20191229153724  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191229153724  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20191229153724  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191229153724                      G�O�G�O�G�O�                JA  ARUP                                                                        20191229155347                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191229153650  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20191229153607  CV  JULD            G�O�G�O�Fǹ                JM  ARCAJMQC2.0                                                                 20200108153412  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200108153412  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200109152352  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124129  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                