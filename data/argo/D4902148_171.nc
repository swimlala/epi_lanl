CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-04-23T18:38:36Z creation;2019-04-23T18:38:40Z conversion to V3.1;2019-12-18T07:15:33Z update;2022-11-21T05:29:03Z update;     
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
resolution        =���   axis      Z        h  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  Md   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ̈   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190423183836  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_171                     2C  Dd qNAVIS_A                         0397                            ARGO 011514                     863 @ظ��
��1   @ظ�/hL @<@���C��d q���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�<�Dπ D�� D�  D�@ D�|�D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@�Q�@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D  �D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D$ �D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�9�D�}DϽD��D�=D�y�DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�y�DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�9�D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D��RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111C�A���A�dZA�G�A�+A��wA���A���A��FA�JA���A�I�A�JA���A��#A���A���A���A���A�l�A�+A�bA��mA��;A���A��DA�^5A��A���A�\)A�-A���A���A�C�A���A�`BA�O�A�Q�A�XA�XA�S�A�+A���A�l�A�VA��A��9A�x�A�XA�(�A�1A��yA�ƨA���A��PA�v�A�dZA�`BA�G�A�+A��`A��jA��FA��9A��9A���A��A�E�A�+A� �A���A���A�"�A�`BA�+A���A�+A�XA��A�33A��A��A��HA��-A�^5A���A��A���A�K�A��AXA~��A~E�A}��A{�Ay/Aw
=Au��As/Ap�jApVAo+AmAm7LAlQ�Ak`BAi��Ah��Agx�AfbAeC�Ab~�A`�/A_�hA^z�A]l�A]33A\�uA[�7AY��AX��AXE�AWK�AVA�AUS�AS��AR��AQ"�AP(�AN�ANbAMƨAL��ALv�AK��AJĜAJ9XAH�AH9XAG�wAG+AF�AD�AB �AA��ABbABE�AA��AA��A@��A@{A?�FA?7LA>=qA=��A=\)A<n�A;|�A:�HA:jA:1'A9��A9"�A8�yA8I�A7"�A6��A5��A4��A4�A4bNA3��A333A2^5A1�^A1/A/�A.�9A-�PA-S�A,�uA,1'A+�wA+t�A+�A*�HA*�\A)A'��A&�+A%�mA%A$ĜA$��A$E�A#�^A#\)A#+A"��A"-A!"�A��A
=A^5AdZA�A?}A�+A{AhsA%A�A��A��AffAA��Al�A�A�wAdZA�HA��A-A�^A��A1'AQ�A�A�AVAA+AA
�A
�A
��A
�DA
M�A	��A	��A	%A��AM�A�TAS�A7LA33A�uAbNAr�A�mA/@�~�@�E�@�@�V@��@��@��@��@�Z@�S�@��@�l�@��T@��@�@�  @�o@�n�@ݩ�@�X@�7L@܋D@��
@��@ڏ\@�J@�O�@�Q�@�?}@�l�@ҟ�@�=q@���@с@мj@��@��@�l�@��#@���@̛�@��@�ff@��@�  @ư!@�O�@�b@�K�@��H@��@�?}@�A�@�S�@�n�@�O�@��D@�9X@�1@��H@��T@��@�`B@�A�@�/@�z�@�9X@���@���@��+@���@��^@�X@� �@��@���@�X@���@�A�@�ƨ@���@�t�@�S�@�"�@�ȴ@���@�5?@���@���@�X@�j@�o@��H@���@��\@��^@��@�G�@���@� �@�~�@���@�j@���@�dZ@�K�@�+@���@���@�~�@���@��D@��;@��@��T@��`@���@��D@�j@�1'@��
@�l�@���@���@�hs@�/@���@��9@��D@� �@��F@�l�@�;d@��@���@�v�@�=q@��@�J@�@�x�@��@�V@��@��@�9X@�dZ@��H@���@�E�@�$�@�{@�J@��@���@�x�@�X@�%@�A�@��F@���@���@�t�@�K�@�C�@�o@��!@�v�@�J@���@���@�p�@��@��9@��u@�A�@�1@���@��P@�l�@�\)@�K�@�C�@�+@��H@���@���@�~�@�V@�$�@��@���@���@�O�@���@���@��@�r�@�Q�@�1@���@�|�@�;d@�
=@��@��H@��H@���@��!@���@�~�@�^5@�E�@���@���@�hs@��@���@���@�bN@�A�@�1@��@~V@}O�@|��@|z�@|Z@|I�@|1@{�@{o@z�!@zn�@y��@y�@x��@x�u@xr�@xQ�@x1'@x1'@x1'@x �@xb@x  @x  @x  @x  @x  @xb@x  @w��@w;d@vE�@u/@t��@sƨ@r��@r~�@q��@qG�@p��@pA�@o�@o�w@o��@o�@n�y@nȴ@n�R@n�y@n�y@n�+@n5?@m��@m��@mO�@mV@l�j@lI�@l1@kƨ@k��@k��@kdZ@j�H@jM�@jJ@i�#@i�7@i�@h�@h1'@h  @g�;@g��@g\)@g�@f�y@fȴ@fv�@f@e�-@e`B@d��@d�j@d�@dj@c��@c@bJ@a��@a&�@` �@`b@_�;@_|�@^��@^v�@^E�@]�T@]�@]/@\�j@\�D@\I�@\(�@\�@[��@[�@[dZ@Z�@Z�\@Z-@Y�7@Y�@XĜ@X �@Wl�@W;d@W+@Vȴ@Vff@V{@U�-@U/@Tz�@TI�@T(�@T�@T1@S�m@Sƨ@S��@S��@S�@SdZ@S@R�\@Q��@Q��@Q�7@Qx�@QX@PA�@Pb@P �@P �@P  @O�;@O\)@Nȴ@N��@Nv�@Nff@N{@M��@M@M@M�-@M��@M��@M�h@Mp�@M`B@MV@L�D@Lj@LZ@L9X@K�
@K�F@K��@KdZ@K@J�H@J��@J��@J�\@JM�@J=q@I�#@IG�@I7L@IG�@H��@H��@H�`@HĜ@H�@G��@Gl�@G
=@F��@F�@Fȴ@Fȴ@F�R@F��@F��@F�+@F�+@FV@F$�@E��@E�h@E`B@E/@D�/@D��@DZ@DZ@DI�@D�@CS�@B��@B-@A��@A��@A�7@A�7@Ax�@Ahs@AX@AG�@A&�@@Ĝ@@r�@@ �@?��@?\)@?+@>��@>�+@>v�@>{@=@=��@=�@=/@<��@<�@<�D@<z�@<z�@<z�@<Z@<9X@;�
@;S�@:��@:M�@9��@9hs@8��@8 �@7�w@7|�@7l�@7l�@7\)@7K�@7;d@7�@6�@6��@6E�@6{@5�@5�@5��@5p�@5O�@4��@4��@4�@4z�@49X@3�
@3dZ@2�@2�\@2M�@2-@2�@1�^@1X@1�@0��@/��@.ff@-��@-�@,�j@,�D@,9X@,(�@,�@+ƨ@+�@+�@+dZ@+C�@+"�@+@*��@*M�@)��@)�#@)�#@)��@)��@)��@)�^@)�^@)��@)X@)7L@)%@)%@(��@(��@(A�@'�@'��@'��@'��@'�w@'�P@'l�@'K�@'+@'�@'
=@&��@&�y@&ȴ@&��@&�+@&ff@&ff@&ff@&V@&$�@&{@&@&@%�@%��@%/@$�j@$��@$z�@$(�@#��@#S�@#C�@#33@#"�@#o@"�@"��@"^5@"=q@"=q@"-@!�@!X@!X@!G�@!7L@!&�@ �`@ bN@ 1'@  �@�w@�@�@v�@�T@�@�@��@1@�@33@�@��@�\@=q@��@��@hs@�@��@1'@�@l�@
=@��@ff@V@5?@{@@@@@��@�-@��@�h@�h@�@`B@�@�/@�j@z�@Z@Z@Z@Z@Z@(�@�@�m@t�@S�@�@��@~�@M�@�@�@��@x�@hs@7L@%@��@��@Q�@  @�@�;@��@�w@|�@+@�R@�+@E�@@�@@��@��@��@�h@p�@/@�@�/@��@�j@��@I�@�m@�
@ƨ@��@�@C�@"�@o@
�@
��@
��@
�!@
�\@
�\@
~�@
~�@
~�@
^5@
=q@	�@	��@	hs@	7L@	&�@	%@��@�@1'@ �@b@b@  @�@�w@+@�y@�+@E�@�T@@`B@/@�j@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111C�A���A�dZA�G�A�+A��wA���A���A��FA�JA���A�I�A�JA���A��#A���A���A���A���A�l�A�+A�bA��mA��;A���A��DA�^5A��A���A�\)A�-A���A���A�C�A���A�`BA�O�A�Q�A�XA�XA�S�A�+A���A�l�A�VA��A��9A�x�A�XA�(�A�1A��yA�ƨA���A��PA�v�A�dZA�`BA�G�A�+A��`A��jA��FA��9A��9A���A��A�E�A�+A� �A���A���A�"�A�`BA�+A���A�+A�XA��A�33A��A��A��HA��-A�^5A���A��A���A�K�A��AXA~��A~E�A}��A{�Ay/Aw
=Au��As/Ap�jApVAo+AmAm7LAlQ�Ak`BAi��Ah��Agx�AfbAeC�Ab~�A`�/A_�hA^z�A]l�A]33A\�uA[�7AY��AX��AXE�AWK�AVA�AUS�AS��AR��AQ"�AP(�AN�ANbAMƨAL��ALv�AK��AJĜAJ9XAH�AH9XAG�wAG+AF�AD�AB �AA��ABbABE�AA��AA��A@��A@{A?�FA?7LA>=qA=��A=\)A<n�A;|�A:�HA:jA:1'A9��A9"�A8�yA8I�A7"�A6��A5��A4��A4�A4bNA3��A333A2^5A1�^A1/A/�A.�9A-�PA-S�A,�uA,1'A+�wA+t�A+�A*�HA*�\A)A'��A&�+A%�mA%A$ĜA$��A$E�A#�^A#\)A#+A"��A"-A!"�A��A
=A^5AdZA�A?}A�+A{AhsA%A�A��A��AffAA��Al�A�A�wAdZA�HA��A-A�^A��A1'AQ�A�A�AVAA+AA
�A
�A
��A
�DA
M�A	��A	��A	%A��AM�A�TAS�A7LA33A�uAbNAr�A�mA/@�~�@�E�@�@�V@��@��@��@��@�Z@�S�@��@�l�@��T@��@�@�  @�o@�n�@ݩ�@�X@�7L@܋D@��
@��@ڏ\@�J@�O�@�Q�@�?}@�l�@ҟ�@�=q@���@с@мj@��@��@�l�@��#@���@̛�@��@�ff@��@�  @ư!@�O�@�b@�K�@��H@��@�?}@�A�@�S�@�n�@�O�@��D@�9X@�1@��H@��T@��@�`B@�A�@�/@�z�@�9X@���@���@��+@���@��^@�X@� �@��@���@�X@���@�A�@�ƨ@���@�t�@�S�@�"�@�ȴ@���@�5?@���@���@�X@�j@�o@��H@���@��\@��^@��@�G�@���@� �@�~�@���@�j@���@�dZ@�K�@�+@���@���@�~�@���@��D@��;@��@��T@��`@���@��D@�j@�1'@��
@�l�@���@���@�hs@�/@���@��9@��D@� �@��F@�l�@�;d@��@���@�v�@�=q@��@�J@�@�x�@��@�V@��@��@�9X@�dZ@��H@���@�E�@�$�@�{@�J@��@���@�x�@�X@�%@�A�@��F@���@���@�t�@�K�@�C�@�o@��!@�v�@�J@���@���@�p�@��@��9@��u@�A�@�1@���@��P@�l�@�\)@�K�@�C�@�+@��H@���@���@�~�@�V@�$�@��@���@���@�O�@���@���@��@�r�@�Q�@�1@���@�|�@�;d@�
=@��@��H@��H@���@��!@���@�~�@�^5@�E�@���@���@�hs@��@���@���@�bN@�A�@�1@��@~V@}O�@|��@|z�@|Z@|I�@|1@{�@{o@z�!@zn�@y��@y�@x��@x�u@xr�@xQ�@x1'@x1'@x1'@x �@xb@x  @x  @x  @x  @x  @xb@x  @w��@w;d@vE�@u/@t��@sƨ@r��@r~�@q��@qG�@p��@pA�@o�@o�w@o��@o�@n�y@nȴ@n�R@n�y@n�y@n�+@n5?@m��@m��@mO�@mV@l�j@lI�@l1@kƨ@k��@k��@kdZ@j�H@jM�@jJ@i�#@i�7@i�@h�@h1'@h  @g�;@g��@g\)@g�@f�y@fȴ@fv�@f@e�-@e`B@d��@d�j@d�@dj@c��@c@bJ@a��@a&�@` �@`b@_�;@_|�@^��@^v�@^E�@]�T@]�@]/@\�j@\�D@\I�@\(�@\�@[��@[�@[dZ@Z�@Z�\@Z-@Y�7@Y�@XĜ@X �@Wl�@W;d@W+@Vȴ@Vff@V{@U�-@U/@Tz�@TI�@T(�@T�@T1@S�m@Sƨ@S��@S��@S�@SdZ@S@R�\@Q��@Q��@Q�7@Qx�@QX@PA�@Pb@P �@P �@P  @O�;@O\)@Nȴ@N��@Nv�@Nff@N{@M��@M@M@M�-@M��@M��@M�h@Mp�@M`B@MV@L�D@Lj@LZ@L9X@K�
@K�F@K��@KdZ@K@J�H@J��@J��@J�\@JM�@J=q@I�#@IG�@I7L@IG�@H��@H��@H�`@HĜ@H�@G��@Gl�@G
=@F��@F�@Fȴ@Fȴ@F�R@F��@F��@F�+@F�+@FV@F$�@E��@E�h@E`B@E/@D�/@D��@DZ@DZ@DI�@D�@CS�@B��@B-@A��@A��@A�7@A�7@Ax�@Ahs@AX@AG�@A&�@@Ĝ@@r�@@ �@?��@?\)@?+@>��@>�+@>v�@>{@=@=��@=�@=/@<��@<�@<�D@<z�@<z�@<z�@<Z@<9X@;�
@;S�@:��@:M�@9��@9hs@8��@8 �@7�w@7|�@7l�@7l�@7\)@7K�@7;d@7�@6�@6��@6E�@6{@5�@5�@5��@5p�@5O�@4��@4��@4�@4z�@49X@3�
@3dZ@2�@2�\@2M�@2-@2�@1�^@1X@1�@0��@/��@.ff@-��@-�@,�j@,�D@,9X@,(�@,�@+ƨ@+�@+�@+dZ@+C�@+"�@+@*��@*M�@)��@)�#@)�#@)��@)��@)��@)�^@)�^@)��@)X@)7L@)%@)%@(��@(��@(A�@'�@'��@'��@'��@'�w@'�P@'l�@'K�@'+@'�@'
=@&��@&�y@&ȴ@&��@&�+@&ff@&ff@&ff@&V@&$�@&{@&@&@%�@%��@%/@$�j@$��@$z�@$(�@#��@#S�@#C�@#33@#"�@#o@"�@"��@"^5@"=q@"=q@"-@!�@!X@!X@!G�@!7L@!&�@ �`@ bN@ 1'@  �@�w@�@�@v�@�T@�@�@��@1@�@33@�@��@�\@=q@��@��@hs@�@��@1'@�@l�@
=@��@ff@V@5?@{@@@@@��@�-@��@�h@�h@�@`B@�@�/@�j@z�@Z@Z@Z@Z@Z@(�@�@�m@t�@S�@�@��@~�@M�@�@�@��@x�@hs@7L@%@��@��@Q�@  @�@�;@��@�w@|�@+@�R@�+@E�@@�@@��@��@��@�h@p�@/@�@�/@��@�j@��@I�@�m@�
@ƨ@��@�@C�@"�@o@
�@
��@
��@
�!@
�\@
�\@
~�@
~�@
~�@
^5@
=q@	�@	��@	hs@	7L@	&�@	%@��@�@1'@ �@b@b@  @�@�w@+@�y@�+@E�@�T@@`B@/@�j@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�1B��B��B�B�yB�5B�B��B�}B�^B�LB�9B�-B�-B�'B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�1B~�B{�B|�B� B�B�B�B�B{�Bs�Br�Bm�BjBiyBm�Bo�Bl�Bl�Bk�BiyBhsBffBdZBdZBbNB_;BXBR�BQ�BQ�BP�BN�BJ�BC�B@�B>wB:^B49B!�BJBB��B�fB��B�B�{Bt�BgmB<jB�B
��B
�B
�B
�jB
��B
�%B
� B
z�B
u�B
o�B
bNB
N�B
>wB
1'B
�B
	7B
B	��B	��B	�B	�B	�`B	�#B	��B	��B	ĜB	�qB	�B	��B	��B	�{B	�\B	�PB	�=B	�%B	~�B	~�B	}�B	z�B	v�B	r�B	l�B	hsB	_;B	W
B	M�B	G�B	D�B	?}B	;dB	8RB	8RB	6FB	0!B	.B	-B	)�B	&�B	�B	hB	{B	!�B	)�B	+B	'�B	$�B	!�B	�B	�B	�B	�B	uB	VB	
=B	1B	%B	B	B	  B��B��B��B�B�B�fB�`B�ZB�HB�5B�B�B��B��BǮBB��B�}B�qB�jB�^B�RB�LB�9B�B��B��B�oB�PB�DB�=B�7B�1B�1B�+B�B�B}�By�Bv�Bt�Bt�Bt�Bt�Bq�Bq�Bo�Bn�Bk�BiyBgmBffBdZBcTBaHB[#BZBXBVBT�BS�BR�BP�BK�BF�BE�BC�BA�B?}B>wB=qB=qB=qB<jB<jB;dB:^B9XB7LB6FB5?B49B33B2-B1'B.B)�B%�B$�B!�B�B�B�B{BoBhBhBbBbB\BVBVBVBVBPBPBPBPBPBPBJBJBJBJBJBJBDBDBJBPBPBPBPBPBVBVBPBPBVB\B\BVBbBbBhBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B!�B#�B+B-B-B/B0!B1'B33B33B33B6FB9XB:^B>wB@�BA�BB�BC�BD�BD�BD�BE�BF�BG�BH�BI�BI�BL�BQ�BR�BR�BR�BVBW
BXBYB[#B`BBcTBgmBjBk�Bk�Bk�Bl�Bl�Bl�Bm�Bt�Bv�Bz�B~�B�B�B�B�%B�+B�1B�=B�PB�uB��B��B��B��B��B��B��B�B�'B�RB�^B�}B��BBBBĜBǮBȴBɺB��B��B�B�B�#B�)B�)B�)B�)B�)B�/B�;B�;B�HB�mB�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	%B	+B	1B		7B	
=B	DB	DB	DB	DB	PB	VB	\B	\B	bB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	&�B	(�B	(�B	)�B	)�B	+B	,B	-B	.B	.B	/B	2-B	49B	6FB	8RB	:^B	;dB	<jB	=qB	>wB	?}B	E�B	J�B	K�B	M�B	N�B	N�B	O�B	Q�B	R�B	R�B	R�B	VB	XB	YB	ZB	[#B	\)B	\)B	\)B	\)B	]/B	]/B	^5B	^5B	^5B	^5B	^5B	^5B	^5B	`BB	bNB	ffB	iyB	k�B	n�B	s�B	u�B	z�B	}�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�7B	�DB	�VB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�FB	�RB	�XB	�dB	�dB	�jB	�qB	�}B	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�BB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
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
PB
VB
VB
VB
VB
VB
bB
oB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
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
-B
-B
-B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
7LB
8RB
9XB
:^B
:^B
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
=qB
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
?}B
@�B
A�B
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
C�B
C�B
C�B
D�B
D�B
D�B
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
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
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
M�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
XB
XB
YB
YB
YB
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
\)B
\)B
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
^5B
^5B
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
bNB
cTB
cTB
cTB
cTB
dZB
cTB
dZB
dZB
e`B
e`B
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
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�1B OB�PB�8B��B�-B�]B�B��B�dB�8B��B�|B��B��B��B�UB��B��B�NB�B�B��B�B�vB�HB��B��B�1B�1B�+B��B�bB�RB�B|B|�B�B�-B�MB��B��B|�Bt9Bs�Bn/BkBi�Bm�Bo�Bl�Bl�Bk�Bi�Bh�Bf�BdtBd�Bb�B_�BXyBSBQ�BRBQ4BO(BK)BC�B@�B>�B;JB5�B#:B6B?B��B�BѝB��B�$Bv�Bj�B?}B�B
�BB
��B
�dB
�oB
�CB
�+B
��B
{�B
w2B
rB
ezB
QhB
@�B
4B
 'B

=B
�B	�wB	��B	��B	��B	�RB	�xB	��B	ϑB	�YB	�iB	�)B	��B	��B	��B	��B	�VB	��B	�KB	�4B	� B	HB	|PB	xRB	t�B	m�B	j0B	`�B	X�B	N�B	HKB	E�B	@iB	<�B	9�B	9rB	7�B	1B	.�B	-�B	+B	)yB	 \B	�B	,B	!�B	*B	+�B	)B	%�B	"hB	 �B	�B	1B	yB	�B	vB	B	�B	�B	�B	�B	 �B��B�PB��B��B�B�B��B�`B�B�VB�	B�
BԕB�pB�B�GBB�4B�B��B��B��B�8B��B�;B�|B��B��B��B��B��B��B��B��B��B�YB��B�Bz�BxBv+BvBv`Bu�Br|Br�BpoBo�Bl�Bj�Bg�BgBd�Bd@Bb�B[�BZ�BX�BV�BU�BT�BS�BR�BM�BGzBF�BEBBuB@4B>�B=�B=�B=�B<�B<�B<B;B:*B7�B6�B6B5B3�B2�B2�B1B,WB'B&�B$�B �B�B�B�BB�B BhB4BbBHBBB�B�B�B�B�B�B�B�B�B�B�B�B�BBJB6BPB�B�B�B�B�B�B�B�BVB�B�B�BvBNB4BTB�BaB9B
BYBEB_BKBQBqB/BBB�B vB"NB#nB# B%�B+�B-]B-�B/�B0�B1�B3�B3�B4B7B9�B;0B?BABBBB�BC�BD�BD�BEBFBG+BHBIBJ#BJ�BM�BR:BS&BSuBS�BVSBWsBX�BY�B\]B`�Bd&Bg�Bj�Bk�Bk�Bk�Bl�Bl�Bm)Bn�BuZBw�B{�B�B�MB�mB�SB�tB��B��B��B��B��B��B�B�B��B�BB� B�>B�IB��B��B��B��B��B��B��B��B�B��B��B�=B�BB҉BؓB�kB�WB�CB�CB�CB�]B�]B�~BߊBߤB��B�B�B��B��B��B�B��B��B�B�B�B�B�<B�HB	 OB	AB	aB	YB	zB	fB		lB	
rB	^B	^B	�B	xB	jB	�B	vB	�B	�B	�B	�B	�B	�B	�B	B	�B	�B	�B	B	 B	$B	%B	'B	)*B	)*B	*B	*B	+6B	,"B	-CB	.IB	.cB	/�B	2aB	4�B	6�B	8�B	:�B	;�B	<�B	=�B	>�B	@B	E�B	J�B	K�B	M�B	OB	OB	PB	R B	S@B	S&B	S[B	V9B	X+B	Y1B	ZQB	[=B	\]B	\CB	\CB	\]B	]dB	]dB	^OB	^5B	^5B	^OB	^5B	^OB	^jB	`�B	b�B	f�B	i�B	k�B	o B	s�B	vB	{0B	~(B	� B	�'B	�AB	�-B	�gB	�9B	�EB	�KB	�1B	�lB	�xB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�>B	�DB	�B	�"B	�WB	�]B	��B	�|B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�(B	�4B	�FB	�gB	�SB	�1B	�KB	�KB	�qB	�xB	�~B	�pB	�B	�ZB	�tB	�zB	�zB	�B	�B	�zB	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�6B	�B	�B	�B	�B	�B	�"B	�"B	�B	�B	�(B	�B	�.B	�B
 4B
 B
UB
[B
-B
B
-B
3B
MB
3B
MB
mB
YB
EB
1B
fB
KB
1B
fB
	7B
	7B
	RB
	7B
	lB

XB

XB
xB
^B
~B
~B
�B
pB
VB
�B
�B
�B
�B
�B
�B
�B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
�B
!B
#B
# B
$&B
%,B
&B
'B
'�B
'�B
'�B
(
B
(
B
(
B
($B
)B
)*B
*B
*B
)�B
*0B
+6B
+B
,"B
,"B
-)B
-)B
-)B
.cB
.IB
/iB
0;B
0UB
1AB
1AB
1vB
2|B
2|B
3hB
3�B
4�B
7�B
8�B
9�B
:�B
:�B
;B
;B
;�B
;B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
>�B
>�B
?�B
?�B
?}B
?�B
?�B
?}B
?�B
?�B
?�B
?�B
@�B
@�B
?�B
@�B
A�B
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
C�B
C�B
C�B
D�B
D�B
D�B
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
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
L�B
MB
N"B
OB
OB
O(B
P.B
Q4B
Q4B
QB
R:B
SB
TB
TB
UB
U2B
UB
U2B
V9B
VSB
W?B
X_B
XEB
YKB
YeB
Y1B
Z7B
[=B
[=B
[=B
[=B
[#B
[#B
[=B
[WB
\CB
\CB
\)B
\)B
\CB
\CB
\]B
\CB
]dB
]IB
^OB
^5B
^OB
^OB
^OB
^jB
^jB
^OB
^jB
_pB
_VB
_VB
`\B
`vB
`vB
`vB
a|B
abB
abB
abB
b�B
b�B
bhB
bhB
cnB
cnB
cTB
c�B
dtB
cnB
d�B
d�B
e�B
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
h�B
h�B
i�B
iyB
i�B
i�B
i�B
j�B
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
l�B
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
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905040031492019050400314920190504003149202211182138462022111821384620221118213846201905050019242019050500192420190505001924  JA  ARFMdecpA19c                                                                20190424033747  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190423183836  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190423183838  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190423183838  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190423183839  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190423183839  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190423183839  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190423183839  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190423183839  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190423183840                      G�O�G�O�G�O�                JA  ARUP                                                                        20190423185638                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190423153200  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190503153149  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190503153149  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190504151924  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123846  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                