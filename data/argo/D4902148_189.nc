CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-20T15:37:43Z creation;2019-10-20T15:37:49Z conversion to V3.1;2022-11-21T05:28:09Z update;     
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
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݨ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20191020153743  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_189                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��]�R�1   @��W; @;���v��dc(���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  BA��BF  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��BA=qBE��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI�\CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D& �D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHs�DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=De �Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�pR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�%A�1A�1A�
=A�
=A�JA�VA�
=A�bA�oA�{A�{A��A�bA��A� �A�$�A�$�A�$�A�"�A�"�A�oA��;A��A�hsA�v�A��FA��!A��A��A��PA��A�%A���A�dZA�`BA�ĜA��wA�\)A�ZA��^A��A�/A��-A���A���A�  A��#A��TA���A�`BA��
A�A�z�A�ZA���A��A��A�oA���A�G�A���A���A�K�A���A�A�A��A�bA���A��!A���A���A���A���A�n�A��A��DA�ffA���A�oA�t�A���A��A�JA���A��A�S�A�%A�oA���A�oAdZA|ȴAzAy/Ax�RAw��Av-As�#Ar�uAp��Ao��AoG�AmAk�PAj�\Ajr�AjAhn�Ag+Ae��Ad��Ab��AaXA_�^A^1'A]A\VA[��AZ$�AYG�AX(�AW�PAUO�AR�ARbAQhsAPz�AO��AO�AO`BAOG�AO&�ANĜANVAL�AK��AI&�AHI�AG��AG�FAG��AG�7AFz�AE/AD�yAD�!AD�AC+ABbNAAXA@�A@(�A?|�A>�/A>1A=�A< �A;�PA:1A8�!A7�#A6ȴA5��A4�!A4A3��A2�A1�7A1K�A1�A0bA.�RA.$�A,�A+�A*�A*I�A)��A(n�A(  A'�FA'�A'�A&�HA&A�A%`BA$�uA"�/A!dZA ��A E�A A�A 5?A {A JA��AJA��A��A��A��A33Ar�A��A�-At�AK�A�AbA��A�A�hA�AȴA+A�AC�A�A
5?A	33A�#A�A�`A��A��A33A�A��A$�A��AXA�uAhsA/A   @�@��`@���@�b@���@��F@�
=@�ȴ@�ff@��^@���@�;d@��#@�bN@�K�@�M�@��@���@�;d@���@�-@�X@��
@噚@�%@䛦@�(�@�P@��@�7L@��m@��@�@�x�@�?}@ܴ9@ۥ�@�M�@�x�@�1'@�dZ@�~�@�x�@��@ԣ�@�Q�@��@�l�@Ώ\@ͺ^@�%@���@�
=@�E�@�G�@��/@�Z@��@�J@ũ�@�X@ă@°!@��j@���@�S�@�@��H@��!@�ff@���@��@��-@�j@�"�@��R@��+@���@���@�A�@���@�
=@���@�|�@��R@�M�@��h@��@��9@�1@��F@�;d@�~�@�E�@���@�7L@��`@��@�A�@��
@�t�@�@�V@��@�p�@�V@��j@��D@�Z@��@�1@�1@�  @��
@�dZ@���@�M�@��@�/@��9@��w@�33@��H@��!@��+@�V@�@��@�1'@�\)@��h@��u@�1@�t�@���@�v�@��@�Ĝ@��D@�1'@��@�t�@�K�@��@�=q@�5?@���@���@���@�hs@��j@� �@��;@��H@�v�@�{@�&�@���@��j@���@�Z@� �@��
@�l�@�
=@��y@��!@�V@���@�`B@�/@���@���@�z�@�A�@��m@��@��@�l�@��@��H@��R@��R@��!@���@���@���@��@���@�hs@�7L@�%@��/@�r�@�Q�@�b@��;@��
@��
@��
@���@���@�t�@�C�@�"�@��@���@�^5@�=q@��T@�O�@���@��/@��9@��j@��@�1@
=@~��@~V@}�@|�@{�@{t�@{C�@z�!@yhs@x��@x��@x�@xQ�@xb@w�;@w\)@w�@w�@v��@v��@vv�@vff@u�@u�@t�D@tj@t�@s�
@s�@s"�@s@r�\@q�#@q�@p��@p�u@pr�@pQ�@p1'@pbN@p1'@o�@p  @o��@o|�@oK�@o
=@n��@nV@n5?@m�T@m�@m?}@l��@l��@l�D@lI�@l1@k��@kC�@j��@j^5@jM�@j-@jJ@i��@i��@iX@h�@g�@g|�@gK�@g�@fȴ@f��@fV@e�-@ep�@e?}@d�@d��@dZ@cS�@a�@`��@`Ĝ@`�9@`bN@_�@^�y@^ȴ@^��@^ff@^$�@]�T@]�h@]p�@]`B@]/@]�@\��@\�/@\�j@\��@\z�@\9X@[�m@[��@[��@[�@[dZ@["�@Z�H@Z�!@Z=q@ZJ@ZJ@Y�@Y��@Y7L@X��@X��@Xr�@W|�@V�+@U�@U`B@U?}@U/@UV@T�/@T9X@S�F@S��@SS�@S@R��@R��@Q�@Qx�@QX@Q�@P�`@P��@Pr�@PQ�@P1'@P �@Pb@O��@O;d@M�-@Mp�@M`B@MO�@M?}@MV@L�@L�D@L�@Kƨ@Kt�@K33@J=q@I��@IG�@I7L@I&�@I�@H�u@G�w@G��@GK�@Fȴ@F��@F��@F$�@E�-@E/@D�/@D�D@DZ@D(�@C�
@C�F@C��@CC�@B~�@BM�@B-@A��@A�@A�#@A��@A��@Ahs@A�@@Ĝ@@Q�@@ �@@  @?�;@?�w@?
=@>�R@>�+@=�T@=��@=�h@=O�@=?}@=V@<��@<�@<��@<j@<I�@;�m@;dZ@:�H@:��@:�\@:�@9�^@9��@9x�@97L@8r�@8  @7�@7�;@7�w@7�w@7�w@7��@7
=@6��@65?@5�@5�@5�T@5��@5O�@5V@4��@4z�@4Z@49X@49X@3�m@333@2�H@2��@2�!@2��@2~�@2^5@2-@2�@2J@1��@1��@1��@1x�@1hs@1%@0�9@0A�@/��@/�w@/|�@/�@.��@.ff@.@-�T@-@-�@-?}@-�@,��@,��@,I�@+�F@+t�@+"�@*n�@*=q@*�@*J@*J@)��@)�#@)��@)7L@(��@(Ĝ@(�9@(�u@(r�@(  @'|�@'\)@'K�@'+@'�@&��@&ȴ@&�+@&ff@&5?@%@%��@%�@$�@$9X@#��@#�@#C�@#"�@#@"�@"�\@"J@!��@!�@ Ĝ@ �u@ 1'@  �@   @��@\)@;d@;d@+@�@��@�y@�@��@v�@E�@@�h@O�@��@��@j@�@�
@��@S�@o@�@�!@~�@^5@J@�7@X@�`@�9@�u@�@Q�@b@  @�;@��@��@K�@+@�@�+@v�@{@@@�h@�h@�@p�@O�@/@V@�@��@�@Z@��@�F@S�@@��@�!@��@=q@�^@X@��@Ĝ@�u@�u@�@A�@1'@b@�@�w@�P@\)@;d@
=@��@�R@v�@�+@v�@ff@V@E�@E�@V@E�@5?@5?@5?@@�T@�T@�T@@�h@p�@�@�@��@�/@��@�j@z�@�@�
@�
@ƨ@��@�@dZ@dZ@S�@33@@
�@
��@
�!@
�!@
��@
~�@
M�@
M�@
J@	�#@	��@	hs@	�@Ĝ@�u@bN@bN@r�@bN@Q�@1'@1'@ �@ �@��@��@��@�P@�P@K�@+@�@
=@��@�@�R@��@�+@�+@5?@{@{@@�T@�h@`B@/@V@V@V@V@�@��@�@��@��@z�@�@��@�m@�
@�
@�
@�F@��@S�@C�@�@�\@~�@~�@n�@n�@M�@�@J@��@�@�@�#@��@x�@X@G�@7L@�@ Ĝ@ ��@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�%A�1A�1A�
=A�
=A�JA�VA�
=A�bA�oA�{A�{A��A�bA��A� �A�$�A�$�A�$�A�"�A�"�A�oA��;A��A�hsA�v�A��FA��!A��A��A��PA��A�%A���A�dZA�`BA�ĜA��wA�\)A�ZA��^A��A�/A��-A���A���A�  A��#A��TA���A�`BA��
A�A�z�A�ZA���A��A��A�oA���A�G�A���A���A�K�A���A�A�A��A�bA���A��!A���A���A���A���A�n�A��A��DA�ffA���A�oA�t�A���A��A�JA���A��A�S�A�%A�oA���A�oAdZA|ȴAzAy/Ax�RAw��Av-As�#Ar�uAp��Ao��AoG�AmAk�PAj�\Ajr�AjAhn�Ag+Ae��Ad��Ab��AaXA_�^A^1'A]A\VA[��AZ$�AYG�AX(�AW�PAUO�AR�ARbAQhsAPz�AO��AO�AO`BAOG�AO&�ANĜANVAL�AK��AI&�AHI�AG��AG�FAG��AG�7AFz�AE/AD�yAD�!AD�AC+ABbNAAXA@�A@(�A?|�A>�/A>1A=�A< �A;�PA:1A8�!A7�#A6ȴA5��A4�!A4A3��A2�A1�7A1K�A1�A0bA.�RA.$�A,�A+�A*�A*I�A)��A(n�A(  A'�FA'�A'�A&�HA&A�A%`BA$�uA"�/A!dZA ��A E�A A�A 5?A {A JA��AJA��A��A��A��A33Ar�A��A�-At�AK�A�AbA��A�A�hA�AȴA+A�AC�A�A
5?A	33A�#A�A�`A��A��A33A�A��A$�A��AXA�uAhsA/A   @�@��`@���@�b@���@��F@�
=@�ȴ@�ff@��^@���@�;d@��#@�bN@�K�@�M�@��@���@�;d@���@�-@�X@��
@噚@�%@䛦@�(�@�P@��@�7L@��m@��@�@�x�@�?}@ܴ9@ۥ�@�M�@�x�@�1'@�dZ@�~�@�x�@��@ԣ�@�Q�@��@�l�@Ώ\@ͺ^@�%@���@�
=@�E�@�G�@��/@�Z@��@�J@ũ�@�X@ă@°!@��j@���@�S�@�@��H@��!@�ff@���@��@��-@�j@�"�@��R@��+@���@���@�A�@���@�
=@���@�|�@��R@�M�@��h@��@��9@�1@��F@�;d@�~�@�E�@���@�7L@��`@��@�A�@��
@�t�@�@�V@��@�p�@�V@��j@��D@�Z@��@�1@�1@�  @��
@�dZ@���@�M�@��@�/@��9@��w@�33@��H@��!@��+@�V@�@��@�1'@�\)@��h@��u@�1@�t�@���@�v�@��@�Ĝ@��D@�1'@��@�t�@�K�@��@�=q@�5?@���@���@���@�hs@��j@� �@��;@��H@�v�@�{@�&�@���@��j@���@�Z@� �@��
@�l�@�
=@��y@��!@�V@���@�`B@�/@���@���@�z�@�A�@��m@��@��@�l�@��@��H@��R@��R@��!@���@���@���@��@���@�hs@�7L@�%@��/@�r�@�Q�@�b@��;@��
@��
@��
@���@���@�t�@�C�@�"�@��@���@�^5@�=q@��T@�O�@���@��/@��9@��j@��@�1@
=@~��@~V@}�@|�@{�@{t�@{C�@z�!@yhs@x��@x��@x�@xQ�@xb@w�;@w\)@w�@w�@v��@v��@vv�@vff@u�@u�@t�D@tj@t�@s�
@s�@s"�@s@r�\@q�#@q�@p��@p�u@pr�@pQ�@p1'@pbN@p1'@o�@p  @o��@o|�@oK�@o
=@n��@nV@n5?@m�T@m�@m?}@l��@l��@l�D@lI�@l1@k��@kC�@j��@j^5@jM�@j-@jJ@i��@i��@iX@h�@g�@g|�@gK�@g�@fȴ@f��@fV@e�-@ep�@e?}@d�@d��@dZ@cS�@a�@`��@`Ĝ@`�9@`bN@_�@^�y@^ȴ@^��@^ff@^$�@]�T@]�h@]p�@]`B@]/@]�@\��@\�/@\�j@\��@\z�@\9X@[�m@[��@[��@[�@[dZ@["�@Z�H@Z�!@Z=q@ZJ@ZJ@Y�@Y��@Y7L@X��@X��@Xr�@W|�@V�+@U�@U`B@U?}@U/@UV@T�/@T9X@S�F@S��@SS�@S@R��@R��@Q�@Qx�@QX@Q�@P�`@P��@Pr�@PQ�@P1'@P �@Pb@O��@O;d@M�-@Mp�@M`B@MO�@M?}@MV@L�@L�D@L�@Kƨ@Kt�@K33@J=q@I��@IG�@I7L@I&�@I�@H�u@G�w@G��@GK�@Fȴ@F��@F��@F$�@E�-@E/@D�/@D�D@DZ@D(�@C�
@C�F@C��@CC�@B~�@BM�@B-@A��@A�@A�#@A��@A��@Ahs@A�@@Ĝ@@Q�@@ �@@  @?�;@?�w@?
=@>�R@>�+@=�T@=��@=�h@=O�@=?}@=V@<��@<�@<��@<j@<I�@;�m@;dZ@:�H@:��@:�\@:�@9�^@9��@9x�@97L@8r�@8  @7�@7�;@7�w@7�w@7�w@7��@7
=@6��@65?@5�@5�@5�T@5��@5O�@5V@4��@4z�@4Z@49X@49X@3�m@333@2�H@2��@2�!@2��@2~�@2^5@2-@2�@2J@1��@1��@1��@1x�@1hs@1%@0�9@0A�@/��@/�w@/|�@/�@.��@.ff@.@-�T@-@-�@-?}@-�@,��@,��@,I�@+�F@+t�@+"�@*n�@*=q@*�@*J@*J@)��@)�#@)��@)7L@(��@(Ĝ@(�9@(�u@(r�@(  @'|�@'\)@'K�@'+@'�@&��@&ȴ@&�+@&ff@&5?@%@%��@%�@$�@$9X@#��@#�@#C�@#"�@#@"�@"�\@"J@!��@!�@ Ĝ@ �u@ 1'@  �@   @��@\)@;d@;d@+@�@��@�y@�@��@v�@E�@@�h@O�@��@��@j@�@�
@��@S�@o@�@�!@~�@^5@J@�7@X@�`@�9@�u@�@Q�@b@  @�;@��@��@K�@+@�@�+@v�@{@@@�h@�h@�@p�@O�@/@V@�@��@�@Z@��@�F@S�@@��@�!@��@=q@�^@X@��@Ĝ@�u@�u@�@A�@1'@b@�@�w@�P@\)@;d@
=@��@�R@v�@�+@v�@ff@V@E�@E�@V@E�@5?@5?@5?@@�T@�T@�T@@�h@p�@�@�@��@�/@��@�j@z�@�@�
@�
@ƨ@��@�@dZ@dZ@S�@33@@
�@
��@
�!@
�!@
��@
~�@
M�@
M�@
J@	�#@	��@	hs@	�@Ĝ@�u@bN@bN@r�@bN@Q�@1'@1'@ �@ �@��@��@��@�P@�P@K�@+@�@
=@��@�@�R@��@�+@�+@5?@{@{@@�T@�h@`B@/@V@V@V@V@�@��@�@��@��@z�@�@��@�m@�
@�
@�
@�F@��@S�@C�@�@�\@~�@~�@n�@n�@M�@�@J@��@�@�@�#@��@x�@X@G�@7L@�@ Ĝ@ ��@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B1B1B1B1B1B	7B1B1B	7B1B1B1B	7B	7B	7B1B1B1B1B1B1B	7B	7BJB�B�;B�B��B��B�RB�B��B��B��B�DB�%B}�Br�Bl�BgmBaHB^5B^5BYBXBVBP�BL�BH�BB�BA�B?}B<jB33B&�B\BB�B�B�B�yB�BB�B��BɺB�wB�LB��B~�By�Bw�Bv�Bv�Bu�Bt�Bo�BhsBM�B,B�BuB1B
��B
�B
�5B
�B
��B
�}B
�XB
�B
��B
��B
�B
r�B
_;B
YB
T�B
K�B
@�B
33B
)�B
�B
�B
�B
\B
B	��B	��B	��B	�B	�yB	�BB	�B	��B	ȴB	�}B	�LB	�'B	�B	��B	��B	��B	�{B	�bB	�B	w�B	r�B	n�B	iyB	ffB	dZB	cTB	bNB	aHB	^5B	ZB	R�B	J�B	<jB	6FB	33B	33B	2-B	1'B	,B	%�B	$�B	"�B	�B	�B	�B	oB	\B	PB		7B	B	  B��B��B�B�B�mB�TB�;B�#B�B�B�
B��B��B��B��BǮBÖB��B�dB�RB�?B�'B�B��B��B��B��B��B��B��B��B��B�oB�DB�+B�+B�+B�+B�+B�%B�Bz�By�Bw�Bv�Bt�Br�Bq�Bo�Bn�Bm�Bl�Bk�BhsBffBbNB_;BZBW
BR�BP�BM�BI�BF�BC�BA�B@�B?}B=qB<jB<jB<jB;dB9XB8RB7LB5?B49B33B1'B0!B0!B/B/B/B.B.B.B-B,B+B+B)�B(�B'�B%�B$�B#�B$�B#�B#�B"�B"�B#�B#�B#�B"�B#�B#�B"�B"�B"�B"�B"�B!�B"�B"�B#�B$�B%�B%�B&�B&�B&�B&�B$�B"�B!�B!�B!�B!�B"�B"�B"�B#�B$�B%�B)�B.B/B1'B1'B33B5?B7LB7LB7LB7LB7LB7LB5?B49B49B6FB7LB9XB;dB<jB?}BB�BD�BI�BJ�BK�BL�BM�BM�BN�BN�BO�BP�BQ�BS�BT�BW
BXBYBZB[#B]/B^5B_;BaHBbNBdZBffBgmBhsBiyBjBjBjBjBk�Bl�Bo�Br�Bu�Bu�Bv�By�B{�B}�B~�B� B�B�B�1B�JB�VB��B��B��B��B��B��B��B�!B�'B�-B�9B�FB�RB�RB�^B�dB�qB�}B��BBBÖBĜBĜBǮBɺB��B��B��B��B��B��B��B��B�B�B�
B�B�)B�BB�HB�TB�`B�`B�mB�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	1B	
=B	VB	\B	bB	bB	bB	bB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	%�B	'�B	,B	-B	.B	2-B	6FB	8RB	8RB	:^B	A�B	F�B	J�B	J�B	K�B	K�B	L�B	M�B	N�B	O�B	O�B	O�B	Q�B	R�B	T�B	XB	\)B	_;B	`BB	bNB	bNB	dZB	gmB	hsB	jB	l�B	n�B	p�B	p�B	p�B	q�B	r�B	t�B	v�B	w�B	y�B	{�B	}�B	� B	�B	�B	�%B	�%B	�%B	�1B	�=B	�DB	�PB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�?B	�FB	�LB	�RB	�jB	�qB	�wB	�}B	��B	��B	��B	��B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�/B	�;B	�;B	�BB	�HB	�NB	�NB	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
%B
%B
+B
1B
	7B

=B
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
hB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
'�B
(�B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
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
6FB
7LB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
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
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
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
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
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
XB
XB
YB
YB
YB
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
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
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
cTB
cTB
dZB
dZB
dZB
e`B
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
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
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
m�B
m�B
m�B
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
o�B
o�B
o�B
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
r�B
r�B
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
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
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
y�B
y�B
z�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B1B1B1B1B1B	RB1B1B	7B1B1B1B	RB	7B	B1B1B1BKB�B	7B�B�B~B�`B�0B�:BּBĜB�B��B��B��B�qB�VB�RB��BuZBo�Bi�Bb�B`'B_�BZ�BZkBW�BS@BO\BJ�BCaBB[B@�B>]B4�B)DBhB�B�hB�B�}B�B�-B�B��B�^B�B�^B�&B�Bz*BxBv�Bv�Bv+Bu�BqvBlqBRB./B!BB	�B
��B
�B
�pB
׍B
ΊB
��B
�JB
�qB
��B
��B
�_B
u�B
`\B
ZB
VmB
NB
CGB
4�B
+�B
!B
�B
�B
�B
-B	�xB	��B	��B	�oB	�QB	��B	یB	ҽB	��B	�UB	��B	�GB	�IB	��B	��B	�/B	��B	�B	��B	x�B	s�B	o�B	jeB	f�B	d�B	c�B	b�B	a�B	_VB	[�B	T�B	M6B	=qB	6�B	3hB	3�B	2�B	2�B	-]B	&fB	%zB	#�B	!B	�B	�B	[B	.B	<B	
XB	YB	UB�6B�B��B�IB��B��B�B�xB�B��B�EBևB�\BΊB�jB�RB��BªB�6B�XB�FB�GB��B��B��B�LB�tB�bB��B��B�B��B�,B�0B��B�_B�_B��B��B�fB��B{�Bz�BxlBw�Bu�Bs�BraBp!BoBnBmCBmBjBgmBc�BaHB[�BX�BT,BR�BO�BK�BH1BESBBuBA B@4B>�B=B<�B=<B<6B:*B9	B8�B6�B4�B4�B2|B0�B0oB/�B/OB/�B.�B.}B.�B-�B,�B,=B,B+B)�B)*B'�B&fB$�B%zB$�B$�B$&B$&B$ZB$ZB$ZB#nB$�B$�B#�B#�B#�B# B#:B"hB#�B#�B$�B%�B&�B&�B'�B'mB'RB'�B&2B$�B"�B"�B"�B"�B#nB#�B#�B$ZB%�B&�B*�B.}B/�B2-B2|B4�B6B7�B7�B7�B7�B8B8�B6�B4�B5%B72B7�B9�B<B=<B@4BC-BEmBJ�BL0BL~BM6BNVBN<BO\BOvBPbBQ�BR�BTFBU�BWsBX_BYeBZkB[�B]�B^�B_�Ba�Bb�Bd�Bf�Bg�Bh�Bi�Bj�Bj�Bj�Bj�BlBm)Bp!BshBvBvFBw�Bz^B|6B~(B.B�OB��B��B�B�B��B�_B�/B�\B�TB�RB�yB��B�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�B�B�XB�6B��B�B�(B�4B�:B�[B�MB�SB�mB�sB�B�xB��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�dB�<B�VB	;B	SB	�B	
�B	pB	�B	�B	�B	}B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	�B	 �B	!�B	%B	&fB	(XB	,WB	-CB	.�B	2�B	6zB	8lB	8�B	:�B	BB	F�B	J�B	J�B	K�B	K�B	MB	NB	OB	PB	O�B	PB	R B	SB	U2B	X_B	\]B	_VB	`�B	bhB	b�B	d�B	g�B	h�B	j�B	l�B	n�B	p�B	p�B	p�B	q�B	r�B	t�B	v�B	xB	zB	|B	~B	�B	�[B	�3B	�YB	�tB	�YB	�fB	�XB	�xB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	�2B	�B	�$B	�0B	�0B	�B	�kB	��B	��B	��B	�zB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ðB	��B	ĶB	ĶB	ĶB	żB	żB	żB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	�B	�4B	�oB	�gB	�?B	�_B	�7B	�7B	�QB	�WB	�WB	�dB	�VB	�VB	�vB	�|B	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�+B	�B	��B	��B	�B	�B	�6B	�6B	�"B	�"B	�BB	�B	�B	�HB
;B
;B
-B
3B
?B
YB
YB
_B
fB
	�B

�B
~B
~B
�B
jB
jB
jB
pB
pB
pB
�B
�B
}B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
#B
$B
%B
$�B
$�B
$�B
&B
%�B
'B
'B
(
B
($B
)*B
($B
)DB
+B
+B
+6B
,"B
,"B
,"B
,"B
-B
-)B
-CB
-)B
-)B
.IB
./B
.cB
/5B
/iB
0UB
1AB
1[B
1[B
2aB
3hB
3MB
4TB
4nB
4nB
5ZB
5tB
5ZB
6zB
6�B
7�B
8lB
8�B
9�B
:�B
:�B
;B
;dB
;�B
;B
;B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
?�B
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
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
IB
H�B
I�B
I�B
J�B
J�B
KB
K�B
K�B
K�B
K�B
K�B
K�B
L�B
MB
L�B
L�B
L�B
N"B
M�B
N�B
O(B
O�B
O�B
O�B
Q B
QB
RB
RB
S&B
S&B
SB
SB
S@B
TFB
UB
U2B
V9B
V9B
V9B
VB
VB
W$B
W$B
W$B
W$B
W?B
X+B
X+B
YKB
YKB
Y1B
Z7B
ZQB
ZQB
[#B
[#B
[=B
[=B
[=B
[WB
[WB
\]B
\CB
\CB
\xB
]dB
]dB
^OB
^jB
^jB
_pB
_pB
_pB
`vB
`vB
a|B
a|B
bNB
bhB
b�B
bNB
bhB
bhB
bhB
cnB
cnB
cnB
cnB
dtB
d�B
dtB
e`B
dZB
ezB
e`B
ezB
e`B
f�B
ffB
f�B
ffB
ffB
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
jB
jB
j�B
j�B
j�B
jB
jB
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
m�B
m�B
m�B
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
o�B
o�B
o�B
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
r�B
r�B
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
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
y	B
x�B
x�B
x�B
x�B
y�B
zB
y�B
zB
zB
y�B
y�B
y�B
z�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<��'<#�
<-��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910310034182019103100341820191031003418202211182140452022111821404520221118214045201911010025102019110100251020191101002510  JA  ARFMdecpA19c                                                                20191021003735  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191020153743  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191020153746  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191020153746  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191020153747  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191020153747  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191020153747  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20191020153747  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20191020153747  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191020153747  QCF$                G�O�G�O�G�O�            8000JA      jafc1.0                                                                 20191020153749                      G�O�G�O�G�O�                JA  ARUP                                                                        20191020155430                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191020153206  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20191020153143  CV  JULD            G�O�G�O�F�-                JM  ARCAJMQC2.0                                                                 20191030153418  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191030153418  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191031152510  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124045  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                