CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-03-08T15:39:38Z creation;2020-03-08T15:39:42Z conversion to V3.1;2022-11-21T05:27:26Z update;     
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
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ά   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200308153938  20221123114511  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_203                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @���\) 1   @��W:� @:�fffff�dv��Fs�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D� RD�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�E�A�G�A�G�A�G�A�I�A�K�A�K�A�I�A�I�A�I�A�K�A�M�A�M�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�"�A��A��A�JA���A���A���A��9A��!A��A���A��uA��\A��7A��A��A��A��A�K�A�A��TA�t�A���A�x�A��-A�l�A�XA�M�A�M�A�-A�ȴA��uA��\A�~�A�O�A�7LA�/A�-A�{A���A�&�A���A�n�A��wA�hsA�
=A���A�l�A�-A��9A��PA��A�33A���A�(�A�  A�hsA�|�A��A�33A���A�\)A��A�XA��;A��A��A�I�A�dZA�hsA��9A��A���A�`BA��A�7LA�%A��A�n�A�VA�=qA�oA� �A�{A��A�33A���A~��A~JA|bNA{+Ay��Av^5As�FAsdZAs
=Ar��Ar�!Ar��ArAq7LApffAm��Ak�Aj��Ai��Ai33Ah�DAg��Ag|�Af��Af�DAeƨAep�Ad��AcAb-Aa��A`�A`�jA`bNA_��A^��A]��A]O�A[�TA[oAY�AX�AX�uAW?}AT��AS�AS?}AQ�#AO�FAMAL��AL�AL�+AL�AK�-AKoAJ�\AIt�AG�
AF^5AEoADr�ACt�AA�FA@z�A?"�A=�;A;�mA;�hA;K�A;?}A;33A;"�A;�A9�A8(�A7VA5t�A4ĜA4bNA3�mA3p�A2ĜA0��A0ffA/��A/\)A-�A*z�A*A)��A)�A)x�A)S�A(�A'�wA%�
A$�\A#�wA#G�A"-A (�A+AȴA9XA��AoA-A/AVA�A�PAp�AXA7LA�yA��AM�A��AC�AA�!A5?A%A��A��A1'A��A��A��A\)A/A�/AȴA�RAbNA+A	��A	�An�A�AM�A+A-A�RA�AXA�A -@�1'@�|�@�{@���@��@�ff@��#@�/@���@�9@�1@�|�@�@�\@�9@�w@�$�@�  @��@�\)@�$�@��@�z�@�9X@���@���@�+@�$�@�@�x�@�G�@�Ĝ@��@���@ݩ�@�1@�v�@��@���@ׅ@�|�@�p�@ϥ�@�C�@��@�v�@��@�1@�|�@�ȴ@�@�A�@�ƨ@Ə\@�(�@�|�@\@�=q@���@��^@���@��P@�
=@�ȴ@�^5@�J@�/@��
@�5?@�r�@��+@�Ĝ@�t�@�%@��@���@���@�r�@��@�
=@�M�@�%@�1@���@�ff@��@�/@�ƨ@���@�-@�7L@� �@��w@��P@�;d@���@�ȴ@���@�-@�hs@�&�@���@�1@��@�o@�ff@��#@�`B@�?}@���@���@�r�@�A�@��
@���@�S�@��@��H@��!@�v�@�E�@�$�@���@���@��@���@�z�@�1@�t�@�"�@��H@���@�ff@��@��h@��@�X@��@���@���@�dZ@�C�@�33@�+@���@��@�&�@�%@��j@�  @�ƨ@��@�S�@��@�o@��@��@���@���@��\@�~�@�~�@�~�@�n�@�-@���@���@��u@��@�S�@���@��@���@�^5@�=q@�-@�=q@��@��T@��-@��7@��@�X@�/@��@��9@��u@�A�@�9X@�I�@�bN@�Z@� �@��@�l�@�~�@��#@���@���@���@���@���@���@�O�@���@��@��@��@��@��`@��u@�I�@�1'@��@
=@~�+@~{@}��@}�@}?}@|�@|��@|z�@|9X@|1@{��@{C�@z��@y�7@xĜ@x1'@w\)@w
=@v��@v�@vE�@v{@up�@t�/@s��@st�@st�@sdZ@sC�@so@r��@r�!@rM�@q��@qhs@q7L@q&�@q%@q%@p�`@p�u@p �@o|�@o
=@n�y@n�@n��@nV@nE�@m�T@m�-@mO�@l��@l�@k��@kdZ@j��@i�#@h��@h�@hQ�@hA�@hA�@g�@dj@c�F@c�@cC�@b�H@b��@b��@b�\@b-@`�`@`b@_�w@_�P@_\)@_;d@^ȴ@^�+@^ff@^E�@^E�@^5?@^$�@^$�@^$�@^@]�@]�T@]�-@\��@[ƨ@[S�@["�@[@Z�\@YX@X��@X�u@Xr�@X  @W�@W\)@W
=@V��@V{@U@U��@U?}@T�/@T��@T�D@TZ@TI�@T(�@S�
@S�@SS�@SS�@S"�@R��@R~�@R=q@RJ@Q�#@Q�^@Qx�@P��@PbN@O�;@N��@N�+@M�T@M�h@M`B@L�@L�@L�D@Lz�@L9X@K��@K�
@K�@Kt�@KC�@J�@J��@J~�@J=q@J�@I��@I��@H�9@Hr�@HQ�@HA�@H �@G�@G�w@G+@G
=@F�@Fȴ@F�+@F$�@F@E�T@E�h@E/@D��@D�/@D��@D�@D�D@Dz�@DZ@D1@C��@Ct�@C"�@B��@BM�@B-@BJ@BJ@A�@A&�@@��@@��@@bN@?��@?��@?K�@>ff@>$�@=�@=�h@=p�@=`B@=?}@=V@<�j@<�D@<9X@;��@;�F@;C�@:�@:�!@:�\@:^5@:�@9�@9�7@9&�@8�u@8  @7�w@7�@7��@7|�@7\)@7;d@7+@7
=@6v�@5�@5@5��@5�h@5p�@5O�@4�@4j@4�@4�@3��@3o@2��@2M�@2J@1��@1�7@1&�@1%@0�`@0Ĝ@0�9@0��@0��@0��@0�@0A�@/�;@/\)@.��@.�y@.�y@.�@.�R@.��@.5?@-�@-`B@-?}@-�@,��@,�/@,�j@,�D@,�D@,�D@,j@,(�@+��@+��@+t�@+dZ@+33@*�@*��@*~�@*-@*J@)��@)��@)%@(�`@(�`@(�`@(Ĝ@(�u@(r�@(1'@(  @'�w@'|�@'+@'
=@&�y@&�y@&�+@&V@&5?@&@$��@$�@$��@$Z@$�@#��@#dZ@#"�@#o@#@#@#@"��@"��@"�!@"�!@"�!@"^5@"=q@"�@!��@!�@!��@!�^@!�^@!��@!�7@!x�@!hs@!%@ Ĝ@ bN@�w@;d@+@�@ȴ@�+@ff@5?@��@`B@V@�D@(�@1@�m@�F@��@S�@33@o@@��@~�@-@�@�7@G�@7L@%@%@�`@��@r�@bN@1'@  @|�@\)@K�@+@�@+@+@+@�@�@ȴ@�R@��@�+@v�@V@{@�T@�-@�h@`B@��@�@z�@I�@�m@�
@ƨ@t�@C�@33@33@"�@��@��@�!@M�@��@hs@�`@��@�9@�u@r�@bN@  @�w@��@l�@\)@+@�@��@$�@�@@@��@�@O�@V@�@��@��@�@�D@(�@�@�@�@��@�F@C�@
��@
~�@
n�@
n�@
M�@
=q@
=q@
=q@
�@
J@	��@	�@	��@	��@	x�@	x�@	G�@	&�@�`@��@Ĝ@�9@�@Q�@1'@�w@�@��@�P@l�@K�@;d@+@+@
=@�@v�@$�@{@{@�@@�@/@�@��@z�@��@��@dZ@S�@"�@�@�!@�\@^5@-@�@J@��@�#@��@hs@%@ ��@ ��@ ��@ �`@ Ĝ@ ��@ �u@ r�@  �@   ?��w?���?��R?�v�?�V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�E�A�G�A�G�A�G�A�I�A�K�A�K�A�I�A�I�A�I�A�K�A�M�A�M�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�"�A��A��A�JA���A���A���A��9A��!A��A���A��uA��\A��7A��A��A��A��A�K�A�A��TA�t�A���A�x�A��-A�l�A�XA�M�A�M�A�-A�ȴA��uA��\A�~�A�O�A�7LA�/A�-A�{A���A�&�A���A�n�A��wA�hsA�
=A���A�l�A�-A��9A��PA��A�33A���A�(�A�  A�hsA�|�A��A�33A���A�\)A��A�XA��;A��A��A�I�A�dZA�hsA��9A��A���A�`BA��A�7LA�%A��A�n�A�VA�=qA�oA� �A�{A��A�33A���A~��A~JA|bNA{+Ay��Av^5As�FAsdZAs
=Ar��Ar�!Ar��ArAq7LApffAm��Ak�Aj��Ai��Ai33Ah�DAg��Ag|�Af��Af�DAeƨAep�Ad��AcAb-Aa��A`�A`�jA`bNA_��A^��A]��A]O�A[�TA[oAY�AX�AX�uAW?}AT��AS�AS?}AQ�#AO�FAMAL��AL�AL�+AL�AK�-AKoAJ�\AIt�AG�
AF^5AEoADr�ACt�AA�FA@z�A?"�A=�;A;�mA;�hA;K�A;?}A;33A;"�A;�A9�A8(�A7VA5t�A4ĜA4bNA3�mA3p�A2ĜA0��A0ffA/��A/\)A-�A*z�A*A)��A)�A)x�A)S�A(�A'�wA%�
A$�\A#�wA#G�A"-A (�A+AȴA9XA��AoA-A/AVA�A�PAp�AXA7LA�yA��AM�A��AC�AA�!A5?A%A��A��A1'A��A��A��A\)A/A�/AȴA�RAbNA+A	��A	�An�A�AM�A+A-A�RA�AXA�A -@�1'@�|�@�{@���@��@�ff@��#@�/@���@�9@�1@�|�@�@�\@�9@�w@�$�@�  @��@�\)@�$�@��@�z�@�9X@���@���@�+@�$�@�@�x�@�G�@�Ĝ@��@���@ݩ�@�1@�v�@��@���@ׅ@�|�@�p�@ϥ�@�C�@��@�v�@��@�1@�|�@�ȴ@�@�A�@�ƨ@Ə\@�(�@�|�@\@�=q@���@��^@���@��P@�
=@�ȴ@�^5@�J@�/@��
@�5?@�r�@��+@�Ĝ@�t�@�%@��@���@���@�r�@��@�
=@�M�@�%@�1@���@�ff@��@�/@�ƨ@���@�-@�7L@� �@��w@��P@�;d@���@�ȴ@���@�-@�hs@�&�@���@�1@��@�o@�ff@��#@�`B@�?}@���@���@�r�@�A�@��
@���@�S�@��@��H@��!@�v�@�E�@�$�@���@���@��@���@�z�@�1@�t�@�"�@��H@���@�ff@��@��h@��@�X@��@���@���@�dZ@�C�@�33@�+@���@��@�&�@�%@��j@�  @�ƨ@��@�S�@��@�o@��@��@���@���@��\@�~�@�~�@�~�@�n�@�-@���@���@��u@��@�S�@���@��@���@�^5@�=q@�-@�=q@��@��T@��-@��7@��@�X@�/@��@��9@��u@�A�@�9X@�I�@�bN@�Z@� �@��@�l�@�~�@��#@���@���@���@���@���@���@�O�@���@��@��@��@��@��`@��u@�I�@�1'@��@
=@~�+@~{@}��@}�@}?}@|�@|��@|z�@|9X@|1@{��@{C�@z��@y�7@xĜ@x1'@w\)@w
=@v��@v�@vE�@v{@up�@t�/@s��@st�@st�@sdZ@sC�@so@r��@r�!@rM�@q��@qhs@q7L@q&�@q%@q%@p�`@p�u@p �@o|�@o
=@n�y@n�@n��@nV@nE�@m�T@m�-@mO�@l��@l�@k��@kdZ@j��@i�#@h��@h�@hQ�@hA�@hA�@g�@dj@c�F@c�@cC�@b�H@b��@b��@b�\@b-@`�`@`b@_�w@_�P@_\)@_;d@^ȴ@^�+@^ff@^E�@^E�@^5?@^$�@^$�@^$�@^@]�@]�T@]�-@\��@[ƨ@[S�@["�@[@Z�\@YX@X��@X�u@Xr�@X  @W�@W\)@W
=@V��@V{@U@U��@U?}@T�/@T��@T�D@TZ@TI�@T(�@S�
@S�@SS�@SS�@S"�@R��@R~�@R=q@RJ@Q�#@Q�^@Qx�@P��@PbN@O�;@N��@N�+@M�T@M�h@M`B@L�@L�@L�D@Lz�@L9X@K��@K�
@K�@Kt�@KC�@J�@J��@J~�@J=q@J�@I��@I��@H�9@Hr�@HQ�@HA�@H �@G�@G�w@G+@G
=@F�@Fȴ@F�+@F$�@F@E�T@E�h@E/@D��@D�/@D��@D�@D�D@Dz�@DZ@D1@C��@Ct�@C"�@B��@BM�@B-@BJ@BJ@A�@A&�@@��@@��@@bN@?��@?��@?K�@>ff@>$�@=�@=�h@=p�@=`B@=?}@=V@<�j@<�D@<9X@;��@;�F@;C�@:�@:�!@:�\@:^5@:�@9�@9�7@9&�@8�u@8  @7�w@7�@7��@7|�@7\)@7;d@7+@7
=@6v�@5�@5@5��@5�h@5p�@5O�@4�@4j@4�@4�@3��@3o@2��@2M�@2J@1��@1�7@1&�@1%@0�`@0Ĝ@0�9@0��@0��@0��@0�@0A�@/�;@/\)@.��@.�y@.�y@.�@.�R@.��@.5?@-�@-`B@-?}@-�@,��@,�/@,�j@,�D@,�D@,�D@,j@,(�@+��@+��@+t�@+dZ@+33@*�@*��@*~�@*-@*J@)��@)��@)%@(�`@(�`@(�`@(Ĝ@(�u@(r�@(1'@(  @'�w@'|�@'+@'
=@&�y@&�y@&�+@&V@&5?@&@$��@$�@$��@$Z@$�@#��@#dZ@#"�@#o@#@#@#@"��@"��@"�!@"�!@"�!@"^5@"=q@"�@!��@!�@!��@!�^@!�^@!��@!�7@!x�@!hs@!%@ Ĝ@ bN@�w@;d@+@�@ȴ@�+@ff@5?@��@`B@V@�D@(�@1@�m@�F@��@S�@33@o@@��@~�@-@�@�7@G�@7L@%@%@�`@��@r�@bN@1'@  @|�@\)@K�@+@�@+@+@+@�@�@ȴ@�R@��@�+@v�@V@{@�T@�-@�h@`B@��@�@z�@I�@�m@�
@ƨ@t�@C�@33@33@"�@��@��@�!@M�@��@hs@�`@��@�9@�u@r�@bN@  @�w@��@l�@\)@+@�@��@$�@�@@@��@�@O�@V@�@��@��@�@�D@(�@�@�@�@��@�F@C�@
��@
~�@
n�@
n�@
M�@
=q@
=q@
=q@
�@
J@	��@	�@	��@	��@	x�@	x�@	G�@	&�@�`@��@Ĝ@�9@�@Q�@1'@�w@�@��@�P@l�@K�@;d@+@+@
=@�@v�@$�@{@{@�@@�@/@�@��@z�@��@��@dZ@S�@"�@�@�!@�\@^5@-@�@J@��@�#@��@hs@%@ ��@ ��@ ��@ �`@ Ĝ@ ��@ �u@ r�@  �@   ?��w?���?��R?�v�?�V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�wB�qB�}B�}B�}B�}B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B�wB�qB�jB�XB�FB�9B�9B�-B�-B�-B�?B�^B�qB�qB�qB�jB�jB�jB�qB�wB�wB�^B�B��B��B�{B�PB�7B�B~�Bz�Bt�Bp�Bo�BjBcTB\)BYBP�B9XB0!B$�B�B�BoB1B�B�B�HBǮB�B��B�oB�Be`BP�BB�B0!BhBB
��B
��B
��B
��B
�sB
��B
�dB
�B
��B
�%B
� B
v�B
l�B
bNB
K�B
:^B
8RB
5?B
33B
2-B
1'B
,B
%�B
�B
\B
B
  B	��B	��B	��B	��B	�B	�B	�B	�B	�mB	�TB	�#B	�B	��B	��B	��B	��B	ǮB	ÖB	�qB	�^B	�9B	�!B	��B	��B	��B	��B	�PB	�+B	�B	y�B	m�B	dZB	`BB	_;B	^5B	[#B	YB	VB	Q�B	K�B	D�B	=qB	8RB	49B	/B	&�B	 �B	�B	{B	PB	DB	
=B	
=B		7B	1B	+B	B��B��B�B�B�sB�fB�TB�;B�B��B��B��BÖB�XB�FB�?B�9B�3B�'B�B��B��B��B��B��B��B�\B�JB�=B�7B�%B�B�B}�B{�By�Bx�Bx�Bw�Bw�Bv�Bu�Bt�Br�Bq�Bp�Bn�Bl�BhsBe`BaHB^5BZBW
BT�BS�BS�BR�BQ�BQ�BO�BL�BI�BG�BF�BC�BA�B>wB<jB9XB8RB7LB5?B2-B/B-B,B+B)�B)�B(�B(�B(�B(�B'�B'�B&�B%�B%�B$�B#�B$�B#�B"�B"�B#�B"�B"�B"�B"�B"�B"�B"�B"�B!�B!�B!�B �B �B �B �B �B�B�B �B!�B"�B"�B"�B"�B!�B"�B"�B"�B"�B#�B"�B#�B&�B'�B(�B(�B(�B(�B)�B,B-B-B-B-B.B.B0!B/B5?B8RB8RB<jBB�BH�BJ�BL�BM�BO�BQ�BT�BXBYB\)B[#B[#BbNBiyBjBn�Bs�Bt�Bu�Bv�Bw�Bx�Bx�Bz�B~�B� B�B�B�B�7B�JB�VB�hB�hB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�9B�RB�^B�^B�dB�qB��BÖBŢBŢBŢBĜBǮB��B��B��B��B�B�B�#B�/B�;B�;B�BB�HB�TB�TB�TB�ZB�ZB�ZB�ZB�fB�sB�B�B��B��B��B	B	B	B	B	+B		7B	
=B	
=B	DB	JB	PB	VB	\B	hB	uB	�B	�B	 �B	"�B	#�B	&�B	,B	.B	/B	0!B	33B	49B	49B	49B	49B	5?B	5?B	8RB	<jB	<jB	=qB	=qB	=qB	=qB	@�B	B�B	B�B	C�B	D�B	D�B	H�B	L�B	N�B	Q�B	T�B	T�B	XB	^5B	_;B	aHB	aHB	dZB	gmB	jB	m�B	o�B	q�B	q�B	q�B	t�B	u�B	w�B	y�B	}�B	~�B	~�B	� B	� B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�7B	�=B	�JB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�?B	�?B	�FB	�LB	�RB	�RB	�RB	�XB	�wB	��B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
1B
	7B
	7B

=B

=B
JB
JB
JB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
bB
hB
hB
oB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
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
"�B
"�B
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
+B
,B
,B
,B
-B
-B
.B
.B
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
0!B
1'B
2-B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
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
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
H�B
H�B
H�B
I�B
I�B
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
L�B
L�B
M�B
M�B
M�B
N�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
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
XB
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
\)B
\)B
]/B
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
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
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
cTB
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
ffB
gmB
gmB
hsB
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
s�B
s�B
s�B
s�B
s�B
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
u�B
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
y�B
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
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�wB��B��B�wB�wB�wB�wB�wB�wB�wB��B��B��B�wB�wB�wB�wB�wB��B�wB�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B��B��B��B�LB��B��B�aB�aB�GB��B�0B��B��B��B��B��B��B��B��B�cB��B��B��B�B��B�<B�	B��B�B{�Bu%Bp�Bp�Bk�BdZB]B[#BTaB:�B1�B&B~B�B{B
�B�B�B�B˒B�cB��B��B��BhXBS�BFB4nB�B�B
�HB
��B
�B
��B
�B
�jB
��B
�)B
��B
��B
��B
x�B
n�B
e�B
NVB
:�B
8�B
5�B
3hB
2�B
1�B
-CB
'�B
"�B
�B
gB
 B	��B	��B	��B	�`B	�B	�UB	�B	�6B	��B	�,B	�CB	ּB	��B	�HB	ϑB	͹B	ȴB	�B	�wB	�B	�tB	��B	�B	��B	��B	�B	��B	��B	�B	|�B	o�B	eFB	`�B	_�B	^�B	[�B	ZB	W
B	S�B	M�B	F�B	>�B	9rB	5�B	1AB	(�B	"�B	~B	mB	�B	�B	
�B	
rB		�B	�B		B	MB��B�zB�}B�B�*B�8B�B�bB��B�B�&B�vBƨB�B��B�tB�nB��B�-B��B�6B�tB��B��B�WB��B��B�B�)B�=B�EB�mB�[BB|�Bz^By$By	Bx8BxRBwLBv`Bu�BshBr-BqvBo�BnIBjBf�Bb�B`vB\xBXEBU�BT{BT{BS[BRTBR�BQ�BN�BJ�BH�BG�BEmBC-B@4B>]B:�B9$B8B7B5B1�B.IB-B,WB*B*�B)yB)DB)_B)�B(sB(sB'�B'8B&�B&B%B%FB$tB#�B#�B$B#:B#TB#�B# B#TB# B# B#:B"NB"�B"�B!�B"B!�B!bB!�B!-B!HB"4B"�B# B# B#nB#�B# B#nB#�B#�B#�B$�B#�B%FB'�B(�B)_B)DB)yB)�B*�B,qB-]B-wB-�B-�B/OB/iB1vB0�B6�B9�B9�B=�BC�BIRBKDBMPBN�BP�BR�BU�BX�BY�B\�B[�B\CBc BjBkQBoOBtBt�Bv+Bw2BxBy	ByrB{BcB��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�'B�hB�2B�8B�XB�eB�cB�iB�vB�|B��B��B��B��B�B��B��B��B��BżB��B�B�KB�JB�B�:BӏB�_B�kB�WB�dB�VBߊB�vB�|B�nB�nB�nB�B�tB�B��B��B�*B�B�GB�?B�*B�(B	uB	[B	GB	SB	_B		lB	
rB	
rB	xB	dB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$B	'B	,qB	.�B	/�B	0�B	3�B	49B	4TB	4TB	4nB	5tB	5�B	8�B	<jB	<�B	=�B	=�B	=�B	=�B	@�B	B�B	B�B	DB	D�B	D�B	H�B	L�B	OB	R:B	U2B	UB	X_B	^OB	_�B	a|B	a�B	d�B	g�B	j�B	m�B	o�B	q�B	q�B	q�B	t�B	vB	x8B	zDB	}�B	B	B	�B	�B	�;B	� B	�[B	�aB	�MB	�YB	�EB	�fB	�RB	�lB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�:B	�LB	�DB	�B	�B	�=B	�=B	��B	��B	��B	�tB	�tB	�zB	��B	�RB	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	ȴB	��B	��B	ɺB	��B	��B	��B	��B	��B	�JB	�(B	� B	�B	�B	�MB	ևB	�_B	�1B	�KB	�eB	�QB	�WB	�xB	�~B	ބB	�vB	�|B	�B	�B	�B	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�	B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�.B	�B	�B	�.B
 iB
'B
-B
3B
9B
9B
?B
tB
fB
	lB
	lB

rB

rB
~B
~B
dB
~B
jB
pB
pB
�B
�B
�B
pB
�B
vB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
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
!B
"�B
#B
$B
#�B
%B
$�B
&B
&B
'B
'8B
($B
(
B
)*B
+B
,"B
,"B
,=B
-CB
-]B
.IB
.IB
.IB
.IB
./B
./B
.cB
.cB
.IB
/OB
/OB
/OB
0;B
1vB
2GB
2GB
2aB
3MB
4TB
4TB
5ZB
5?B
5ZB
6`B
6FB
6`B
7�B
7�B
8�B
8lB
9XB
9rB
9rB
9�B
9rB
9�B
:�B
;�B
;�B
;B
;B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
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
E�B
E�B
E�B
E�B
E�B
F�B
F�B
GB
H�B
H�B
H�B
I�B
I�B
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
MB
MB
L�B
L�B
M�B
NB
M�B
N�B
PB
Q B
Q B
QB
RB
RB
R B
S&B
TFB
T�B
U2B
UB
U2B
VB
V9B
V9B
VSB
W$B
WYB
X+B
Y1B
Y1B
Z7B
Z7B
ZQB
Z7B
[=B
[WB
[=B
[=B
\CB
\]B
]IB
^OB
^OB
^OB
^5B
^OB
^OB
_pB
_pB
_VB
_VB
_pB
`\B
`\B
aHB
abB
aHB
aHB
aHB
a|B
abB
aHB
abB
aHB
a|B
abB
a|B
b�B
bhB
bhB
bhB
b�B
b�B
c�B
d�B
d�B
d�B
ezB
ezB
e�B
ezB
ffB
f�B
f�B
f�B
ffB
f�B
f�B
g�B
g�B
h�B
i�B
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
s�B
s�B
s�B
s�B
s�B
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
u�B
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
y	B
y	B
y�B
z�B
z�B
z�B
z�B
|B
|B
|B
}"B
|�B
}B
|�B
}"B
}B
}B
~(B
}�B
~B
~B
~B
~B
~(B
}�B
.B
.B
B
.B
B
� B
�B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202003200035242020032000352420200320003524202211182142192022111821421920221118214219202003210017502020032100175020200321001750  JA  ARFMdecpA19c                                                                20200309003836  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200308153938  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200308153940  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200308153941  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200308153941  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200308153941  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200308153941  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200308153941  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200308153941  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200308153941  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200308153942  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200308153942                      G�O�G�O�G�O�                JA  ARUP                                                                        20200308155409                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200308153402  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200308153338  CV  JULD            G�O�G�O�F�E                JM  ARCAJMQC2.0                                                                 20200319153524  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200319153524  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200320151750  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124219  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114511                      G�O�G�O�G�O�                