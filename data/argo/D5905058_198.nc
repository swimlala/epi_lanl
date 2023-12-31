CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-24T03:37:19Z creation;2019-12-24T03:37:23Z conversion to V3.1;2023-06-29T05:50:24Z update;     
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
resolution        =���     �  Mx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �H   PSAL_ADJUSTED_QC         
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191224033719  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_198                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @����L��1   @���5� @7��|����b�b��}V1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D���D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Mp�@��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B���B�k�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C 5�CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�MD���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƸD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D�D���D�	�D�I�DÉ�D���D�	�D�I�Dĉ�D���D�	�D�I�Dŉ�D���D�	�D�I�DƉ�D���D�	�D�I�Dǉ�D���D�	�D�I�Dȉ�D���D�	�D�I�Dɉ�D���D�	�D�I�Dʉ�D���D�	�D�I�Dˉ�D���D�	�D�I�D̉�D���D�	�D�I�D͉�D���D�	�D�I�DΉ�D���D�	�D�I�Dω�D���D��D�I�DЉ�D���D�	�D�I�Dщ�D���D�	�D�I�D҉�D���D�	�D�I�DӉ�D���D�	�D�I�Dԉ�D���D�	�D�I�DՉ�D���D�	�D�I�D։�D���D�	�D�I�D׉�D���D�	�D�I�D؉�D���D�	�D�I�Dى�D���D�	�D�I�Dډ�D���D�	�D�I�Dۉ�D���D�	�D�I�D܉�D���D�	�D�I�D݉�D���D�	�D�I�Dމ�D���D�	�D�I�D߉�D��D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D��D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aʧ�Aʩ�Aʧ�Aʩ�Aʩ�AʬAʮAʮAʮAʰ!Aʲ-Aʲ-Aʲ-Aʴ9Aʴ9AʶFAʴ9AʶFAʶFAʸRAʸRAʺ^Aʺ^AʼjAʼjAʾwA���A���A�A�ĜA�ȴA�ȴA���A���A���A���A���A���A��
A��
A��
A��
A��
A��
A��A���A�dZA�jAț�A�7LA�t�A��\A���A�?}A��TA�"�A�~�A�1'A��`A�^5A�x�A�l�A��TA��A���A�r�A���A��A�1'A��A� �A���A��A���A���A��RA�&�A���A�t�A�O�A���A�jA�|�A�1'A��A��FA�O�A�{A�bA���A�l�A�1'A���A�bA��A��9A��A�r�A�ĜA���A��-A��A��A���A��A��TA��A��A�$�A`BA~�A~��A}p�A|5?Azz�Ax(�Au��At�!AsK�Apn�Al��Ai��Ag�hAe�7Ac\)A_��A]O�A\�\A[��AY�
AV5?ASO�AT �ASS�AP1AO
=AM�
ALv�AI`BAF  ABffA?`BA>ZA<��A;C�A8ĜA7��A6�RA6jA6 �A5�PA4z�A3�PA2�A1��A/��A0Q�A/+A-��A+�mA+�7A*��A(�A'��A'%A&~�A&1'A&1'A%"�A$~�A#�;A#hsA!��A �/A��A�`A�Az�A�FA��A�TA
=A��A�A�/A(�AXA��A(�Ax�AjA33A�yAjA7LA�\A1AAt�A�!AXA
�A	��A	;dA	7LA�`AE�A|�A�9A��A�PAS�Ar�A �A�wA�A%AA �/A 1@�C�@�x�@��9@�
=@��@�hs@��@�p�@�A�@�@��y@�\@�^5@�I�@�+@�/@�I�@�S�@�p�@�9X@�t�@��@��@�@ܛ�@�t�@ۅ@�hs@�|�@��@�  @���@�l�@ҟ�@���@��@���@�E�@���@�~�@��@͡�@�7L@�p�@�b@ˮ@�+@�-@�t�@���@�-@�M�@��/@��
@��m@Ĭ@�z�@� �@�b@ģ�@���@�  @�S�@���@�ff@�{@�%@���@�Ĝ@�?}@��#@�V@���@���@���@��w@�@���@�X@�b@�`B@��^@�9X@�C�@�+@��#@�@��T@���@�(�@��@�ȴ@�%@�A�@���@��R@�z�@� �@��@�M�@�+@��y@�=q@��@��D@�(�@�|�@��R@��^@��u@�Q�@��;@�Z@���@�G�@�ƨ@��R@��@��@�hs@�^5@�&�@��@��@���@�ȴ@��R@�33@�Z@��u@�r�@�1'@��j@��9@���@�@�v�@��\@�o@�"�@��\@��-@�7L@��j@���@�l�@�|�@���@��@�K�@�V@�33@���@�&�@�-@�=q@�%@�bN@��@��j@���@�(�@�ƨ@���@���@�A�@�C�@�o@���@�=q@�/@�%@�/@��7@��^@��T@���@�/@�9X@�1@���@���@�|�@�S�@�S�@�~�@�v�@���@���@�@��F@�9X@�  @���@���@��@�$�@���@��-@��T@���@�`B@�&�@�%@��@�ȴ@��+@�
=@���@���@���@��@�O�@��@���@��9@�j@�(�@��m@�ƨ@���@�;d@�o@���@�V@��@�@��@���@���@���@�G�@���@��@���@�r�@�I�@�(�@�1@��@�ƨ@���@�|�@�C�@�"�@��y@���@��!@���@�n�@�J@��T@��#@��-@�O�@���@��j@��@�(�@�  @��m@���@�ƨ@��F@��@���@�|�@�dZ@�K�@�+@���@��R@�v�@�E�@��@���@���@�@�hs@�/@��@���@���@�r�@��@�P@|�@\)@~��@~ȴ@~V@}�T@}��@}��@|��@|j@|9X@{�m@{t�@{"�@z��@z~�@z-@y��@yhs@yG�@y&�@x�`@x��@x�@xQ�@x �@w�@w��@wl�@w\)@wK�@w�@w
=@v�y@v�+@vE�@v5?@u@u�@uO�@t��@t9X@s��@sC�@s"�@r�H@r�\@q�#@q7L@p��@p1'@o��@o�@o+@nv�@nV@n{@m�@l��@l��@lZ@k��@k��@j�@j�!@jM�@i��@iX@i7L@h��@h��@h1'@h  @g\)@g
=@f�y@f�@f�+@e�@ep�@d�j@d�D@dj@dI�@d1@c�m@c��@c"�@b��@b�\@b=q@a�#@ax�@a�@`��@`��@` �@_��@_��@_|�@^��@^�R@^{@]��@]��@]`B@\�@\9X@\�@[�m@["�@Z��@Z�@Y�7@Y�@X��@W�@W��@W�@Vȴ@V�+@VE�@V5?@U�T@UO�@U/@T��@T�D@TZ@S��@Sƨ@S�@S33@R��@R=q@Q�#@Q�^@Qx�@Q�@Pr�@P1'@O��@O|�@O;d@N�@Nv�@N5?@N{@M�T@M�-@M/@MV@L�/@L�j@L�D@LI�@L1@KS�@Ko@K@J�H@J��@Jn�@J-@I�@I��@I�7@I7L@H�9@H�@H�@G�@G|�@G
=@Fȴ@F��@F��@FV@F5?@E��@E�@E/@D�/@D�j@D�D@D(�@C�
@Ct�@Co@B�@B��@B^5@B-@A�^@A��@Ax�@Ahs@AG�@A7L@A�@@�`@@bN@?�;@?��@?|�@?K�@?+@>��@>�R@>ff@=@=p�@=`B@=V@<�D@<I�@<�@;�
@;��@;t�@;33@:�@:�\@:�@9�#@9��@9��@9��@9��@9X@9�@8�`@8�u@81'@8  @7�w@7�P@7;d@7
=@6ȴ@6v�@6V@65?@6$�@6@5�T@5�h@5/@4��@4�D@49X@4(�@3�
@3��@3C�@3@2�H@2��@2��@2-@1��@1�#@1��@1hs@1X@1&�@1%@0�`@0Ĝ@0��@0�@0r�@0Q�@/��@/\)@/+@/
=@.�y@.�@.��@.@-/@,�j@,��@,�D@,j@,Z@,I�@+��@+�F@+��@+dZ@+C�@+@*�@*�H@*��@*^5@*M�@)�@)��@)G�@)�@(��@(��@(�9@(r�@(b@'��@'�@'��@'l�@';d@&�y@&��@&��@&v�@&$�@&{@%�@%�T@%��@%��@%p�@%V@$�@$�/@$�D@$1@#�m@#�F@#�@#S�@#33@#o@"�@"�!@"~�@"n�@"=q@"J@!�@!��@!��@!G�@ ��@ ��@ ��@ ��@ �@ bN@ A�@�@�P@;d@��@ȴ@ff@{@�@��@O�@/@/@��@�@��@��@Z@�@��@�m@�m@�m@�
@�
@ƨ@��@�@�@�@t�@dZ@dZ@S�@o@��@�\@=q@�@��@hs@7L@&�@��@�9@r�@A�@1'@�@��@�@l�@+@
=@�y@ȴ@��@�+@ff@V@5?@$�@{@@@�@�@��@@��@�@O�@�@V@�@��@��@Z@9X@�@��@��@�m@ƨ@��@dZ@dZ@33@"�@�H@�!@�\@�\@�\@n�@=q@�@�^@��@�7@hs@X@7L@&�@%@Ĝ@�@1'@  @�;@�w@��@��@�P@|�@K�@+@
=@��@��@�@��@�+@�+@V@{@@�@�-@�@p�@`B@O�@/@�@�@�@�j@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aʧ�Aʩ�Aʧ�Aʩ�Aʩ�AʬAʮAʮAʮAʰ!Aʲ-Aʲ-Aʲ-Aʴ9Aʴ9AʶFAʴ9AʶFAʶFAʸRAʸRAʺ^Aʺ^AʼjAʼjAʾwA���A���A�A�ĜA�ȴA�ȴA���A���A���A���A���A���A��
A��
A��
A��
A��
A��
A��A���A�dZA�jAț�A�7LA�t�A��\A���A�?}A��TA�"�A�~�A�1'A��`A�^5A�x�A�l�A��TA��A���A�r�A���A��A�1'A��A� �A���A��A���A���A��RA�&�A���A�t�A�O�A���A�jA�|�A�1'A��A��FA�O�A�{A�bA���A�l�A�1'A���A�bA��A��9A��A�r�A�ĜA���A��-A��A��A���A��A��TA��A��A�$�A`BA~�A~��A}p�A|5?Azz�Ax(�Au��At�!AsK�Apn�Al��Ai��Ag�hAe�7Ac\)A_��A]O�A\�\A[��AY�
AV5?ASO�AT �ASS�AP1AO
=AM�
ALv�AI`BAF  ABffA?`BA>ZA<��A;C�A8ĜA7��A6�RA6jA6 �A5�PA4z�A3�PA2�A1��A/��A0Q�A/+A-��A+�mA+�7A*��A(�A'��A'%A&~�A&1'A&1'A%"�A$~�A#�;A#hsA!��A �/A��A�`A�Az�A�FA��A�TA
=A��A�A�/A(�AXA��A(�Ax�AjA33A�yAjA7LA�\A1AAt�A�!AXA
�A	��A	;dA	7LA�`AE�A|�A�9A��A�PAS�Ar�A �A�wA�A%AA �/A 1@�C�@�x�@��9@�
=@��@�hs@��@�p�@�A�@�@��y@�\@�^5@�I�@�+@�/@�I�@�S�@�p�@�9X@�t�@��@��@�@ܛ�@�t�@ۅ@�hs@�|�@��@�  @���@�l�@ҟ�@���@��@���@�E�@���@�~�@��@͡�@�7L@�p�@�b@ˮ@�+@�-@�t�@���@�-@�M�@��/@��
@��m@Ĭ@�z�@� �@�b@ģ�@���@�  @�S�@���@�ff@�{@�%@���@�Ĝ@�?}@��#@�V@���@���@���@��w@�@���@�X@�b@�`B@��^@�9X@�C�@�+@��#@�@��T@���@�(�@��@�ȴ@�%@�A�@���@��R@�z�@� �@��@�M�@�+@��y@�=q@��@��D@�(�@�|�@��R@��^@��u@�Q�@��;@�Z@���@�G�@�ƨ@��R@��@��@�hs@�^5@�&�@��@��@���@�ȴ@��R@�33@�Z@��u@�r�@�1'@��j@��9@���@�@�v�@��\@�o@�"�@��\@��-@�7L@��j@���@�l�@�|�@���@��@�K�@�V@�33@���@�&�@�-@�=q@�%@�bN@��@��j@���@�(�@�ƨ@���@���@�A�@�C�@�o@���@�=q@�/@�%@�/@��7@��^@��T@���@�/@�9X@�1@���@���@�|�@�S�@�S�@�~�@�v�@���@���@�@��F@�9X@�  @���@���@��@�$�@���@��-@��T@���@�`B@�&�@�%@��@�ȴ@��+@�
=@���@���@���@��@�O�@��@���@��9@�j@�(�@��m@�ƨ@���@�;d@�o@���@�V@��@�@��@���@���@���@�G�@���@��@���@�r�@�I�@�(�@�1@��@�ƨ@���@�|�@�C�@�"�@��y@���@��!@���@�n�@�J@��T@��#@��-@�O�@���@��j@��@�(�@�  @��m@���@�ƨ@��F@��@���@�|�@�dZ@�K�@�+@���@��R@�v�@�E�@��@���@���@�@�hs@�/@��@���@���@�r�@��@�P@|�@\)@~��@~ȴ@~V@}�T@}��@}��@|��@|j@|9X@{�m@{t�@{"�@z��@z~�@z-@y��@yhs@yG�@y&�@x�`@x��@x�@xQ�@x �@w�@w��@wl�@w\)@wK�@w�@w
=@v�y@v�+@vE�@v5?@u@u�@uO�@t��@t9X@s��@sC�@s"�@r�H@r�\@q�#@q7L@p��@p1'@o��@o�@o+@nv�@nV@n{@m�@l��@l��@lZ@k��@k��@j�@j�!@jM�@i��@iX@i7L@h��@h��@h1'@h  @g\)@g
=@f�y@f�@f�+@e�@ep�@d�j@d�D@dj@dI�@d1@c�m@c��@c"�@b��@b�\@b=q@a�#@ax�@a�@`��@`��@` �@_��@_��@_|�@^��@^�R@^{@]��@]��@]`B@\�@\9X@\�@[�m@["�@Z��@Z�@Y�7@Y�@X��@W�@W��@W�@Vȴ@V�+@VE�@V5?@U�T@UO�@U/@T��@T�D@TZ@S��@Sƨ@S�@S33@R��@R=q@Q�#@Q�^@Qx�@Q�@Pr�@P1'@O��@O|�@O;d@N�@Nv�@N5?@N{@M�T@M�-@M/@MV@L�/@L�j@L�D@LI�@L1@KS�@Ko@K@J�H@J��@Jn�@J-@I�@I��@I�7@I7L@H�9@H�@H�@G�@G|�@G
=@Fȴ@F��@F��@FV@F5?@E��@E�@E/@D�/@D�j@D�D@D(�@C�
@Ct�@Co@B�@B��@B^5@B-@A�^@A��@Ax�@Ahs@AG�@A7L@A�@@�`@@bN@?�;@?��@?|�@?K�@?+@>��@>�R@>ff@=@=p�@=`B@=V@<�D@<I�@<�@;�
@;��@;t�@;33@:�@:�\@:�@9�#@9��@9��@9��@9��@9X@9�@8�`@8�u@81'@8  @7�w@7�P@7;d@7
=@6ȴ@6v�@6V@65?@6$�@6@5�T@5�h@5/@4��@4�D@49X@4(�@3�
@3��@3C�@3@2�H@2��@2��@2-@1��@1�#@1��@1hs@1X@1&�@1%@0�`@0Ĝ@0��@0�@0r�@0Q�@/��@/\)@/+@/
=@.�y@.�@.��@.@-/@,�j@,��@,�D@,j@,Z@,I�@+��@+�F@+��@+dZ@+C�@+@*�@*�H@*��@*^5@*M�@)�@)��@)G�@)�@(��@(��@(�9@(r�@(b@'��@'�@'��@'l�@';d@&�y@&��@&��@&v�@&$�@&{@%�@%�T@%��@%��@%p�@%V@$�@$�/@$�D@$1@#�m@#�F@#�@#S�@#33@#o@"�@"�!@"~�@"n�@"=q@"J@!�@!��@!��@!G�@ ��@ ��@ ��@ ��@ �@ bN@ A�@�@�P@;d@��@ȴ@ff@{@�@��@O�@/@/@��@�@��@��@Z@�@��@�m@�m@�m@�
@�
@ƨ@��@�@�@�@t�@dZ@dZ@S�@o@��@�\@=q@�@��@hs@7L@&�@��@�9@r�@A�@1'@�@��@�@l�@+@
=@�y@ȴ@��@�+@ff@V@5?@$�@{@@@�@�@��@@��@�@O�@�@V@�@��@��@Z@9X@�@��@��@�m@ƨ@��@dZ@dZ@33@"�@�H@�!@�\@�\@�\@n�@=q@�@�^@��@�7@hs@X@7L@&�@%@Ĝ@�@1'@  @�;@�w@��@��@�P@|�@K�@+@
=@��@��@�@��@�+@�+@V@{@@�@�-@�@p�@`B@O�@/@�@�@�@�j@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�)B
�/B
�/B
�5B
�HB
�HB
�NB
�HB
�HB
�HB
�NB
�`B
�fB
�mB
�sB
�sB
�yB
�B
�B
�B�B_;Be`Bw�B�7B�B"�B
�B
�5B$�B{�B��BJB(�B0!BA�BF�BI�BW
BaHBW
BVBN�B6FB6FB?}B?}B?}BA�BN�BL�BI�BC�B<jB6FB49B0!B,B1'B&�B�B��B�B�mB�)B�
B�B��BB�'B��B�%BK�B49B �B
��B
�'B
~�B
hsB
C�B
�B	�B	�B	��B
{B
'�B
�B
�B
hB
B	�B	�`B	�B	ɺB	�B	��B	�DB	w�B	_;B	7LB	#�B	�B	�B	{B�B�B�B�B�)B��B��BƨB�wB�9B�B��B��B��B��B�VB�=B�1B�%B�B�B�B|�Bz�Bv�Bm�B�B�+B�1B}�By�Bw�Bl�BiyBhsBw�By�B�B�%B�%B�1B�1B�Bz�Bw�Bt�Bn�BcTBaHBbNBaHB`BB^5B_;B]/B\)BZBYBW
BT�BS�BR�BQ�BQ�BR�BVBW
BXBXBVBQ�BQ�BS�BW
BXBXBVBS�BVBR�BQ�BVBYBYB[#B\)BbNB_;BXBQ�BYBQ�BXBVBW
BaHBQ�BXBW
BN�BH�BI�BM�BP�BF�BE�BH�BG�BI�BK�BJ�BJ�BM�BL�BM�BM�BVBR�BO�BN�BM�BM�BM�BR�BVB_;BffBgmBcTB]/B]/BaHBbNBhsBhsBe`Be`Bk�BgmBgmBgmBn�Bp�Bq�Bx�B�B�DB�PB�VB�{B��B��B��B��B��B��B�B�B�B�-B�jB�^B�LB�dB��BĜB�}B�^BB��B��B�B�B�
B�B�B�TB�mB�fB�fB�mB�B�fB�fB�mB�fB�BB�BB�B�yB�;B�NB�NB�ZB�TB�`B�yB�B�B�B�B�B��B��B	B��B��B��B��B	B	PB	PB	bB	�B	!�B	%�B	&�B	+B	:^B	B�B	D�B	E�B	J�B	P�B	O�B	N�B	N�B	S�B	]/B	_;B	^5B	]/B	[#B	ZB	[#B	ZB	]/B	^5B	ZB	Q�B	M�B	T�B	\)B	gmB	p�B	r�B	r�B	r�B	x�B	{�B	}�B	� B	�B	�B	}�B	x�B	u�B	u�B	t�B	w�B	v�B	w�B	y�B	}�B	�B	�B	�+B	�7B	�+B	�+B	�1B	�JB	�PB	�\B	�{B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�B	�B	�B	�'B	�-B	�B	�!B	�'B	�3B	�?B	�FB	�FB	�RB	�^B	�jB	�jB	�qB	�}B	�}B	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�#B	�/B	�;B	�BB	�BB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
JB
PB
PB
PB
PB
PB
VB
VB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
-B
.B
.B
.B
.B
.B
/B
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
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
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
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
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
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
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
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
XB
YB
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
dZB
dZB
dZB
e`B
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
iyB
iyB
iyB
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
p�B
p�B
p�B
q�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�-B
�-B
�4B
�-B
�-B
�-B
�4B
�FB
�LB
�RB
�XB
�XB
�_B
�B
��B
�tB!bBb4BlWB}�B�B��B'�B
�UB
ޞB$�B|�B�2B(B+QB4TBD�BH�BL�BY�BdtBX�BXBQB7�B6�B?}B?�B@BCaBP.BNpBL0BE�B=�B72B5�B1B-�B3�B)�B�B��B��B�yB��B�B�EB҉B�SB��B�IB�xBN�B7B$�B
��B
��B
��B
l�B
G�B
!�B	��B	�B	�cB
�B
)�B
 vB
�B
B
�B	�B	�B	��B	��B	�aB	�xB	��B	z�B	c B	9�B	$�B	VB	"�B	_B�-B�EB��B��BݲB��B�.B��B��B�lB�!B�TB��B��B�B��B�DB��B��B�B�mB�-B~B|�Bx8Bm�B��B�B��B~�B{Bz*Bm�Bj0BiBxBzDB�AB��B��B�B��B��B|jBx�BvFBpBdtBb�Bc:BbNB`�B_!B`vB^5B]/BZ�BY�BXBVmBUMBS[BR�BS@BS�BV�BWsBX�BY1BWsBR�BR�BT�BW?BX�BX�BV�BT�BV�BS�BRoBV�BY�BY�B[�B\xBcnB`�BYBR�BY�BR�BX�BV�BX_Bb�BRoBY1BYBO�BIBJrBO�BS�BG�BFYBIlBH�BJrBL0BKBK)BN"BM�BNpBN<BWYBTBP�BO�BM�BN"BN<BR�BU�B_pBf�Bh�Bd&B]~B]dBa|BbhBi*Bh�Be�BfLBl�Bg�Bg�Bg�BoOBp�BqvBx�B�'B�DB�B��B��B�&B�&B�&B�B�,B�mB�B��B��B�B��B��B�2B�dB��BŢB� B�xB��B��B��B��BچB�?BٴB�B�nB��B��B��B�
B�QB��B�B�>B�mB�vB�vB�B��BߤB�B�B�ZB�B��B��B�B�B�B�B�/B�TB��B	�B�jB�DB�B��B	�B	�B	6B	�B	B	!|B	%�B	&�B	*B	:B	B�B	D�B	EmB	J�B	QNB	PHB	N�B	N�B	S�B	]B	_�B	^�B	]dB	[qB	ZkB	[WB	ZB	]~B	_pB	[#B	R B	MPB	TFB	[WB	f�B	p�B	s3B	r�B	r|B	x�B	|B	~(B	�OB	��B	��B	~�B	y>B	u�B	u�B	uB	xRB	v�B	w�B	y�B	}�B	��B	�B	�zB	��B	�+B	�_B	�1B	�JB	�PB	�vB	��B	�TB	�2B	�EB	��B	�BB	��B	�0B	��B	�$B	��B	��B	�B	��B	��B	�!B	�;B	�!B	�UB	��B	�cB	��B	��B	�'B	��B	�OB	�!B	�'B	�MB	�%B	�FB	�`B	�lB	�^B	�PB	�jB	��B	�}B	��B	��B	ÖB	āB	ňB	ňB	ƨB	ƨB	��B	��B	��B	ˬB	��B	��B	��B	οB	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�$B	�B	�B	�B	�=B	�IB	�VB	�\B	�\B	�NB	�:B	�:B	�:B	�ZB	�@B	�@B	�@B	�`B	�`B	�`B	�fB	�mB	�mB	�sB	�_B	�yB	�yB	�yB	�B	�B	�qB	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
 �B
B
 �B
 B
B
�B
B
B
B
�B
B
�B
9B
9B
?B
%B
+B
1B
B
1B
	7B
	7B
	7B
	RB
	RB
	RB
	B
	7B

=B
DB
DB
^B
dB
PB
PB
PB
PB
PB
VB
pB
bB
NB
NB
�B
hB
hB
hB
TB
TB
TB
TB
TB
oB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
sB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(
B
'�B
(
B
(�B
(�B
)�B
)�B
*�B
+B
*�B
*�B
+B
,B
,B
,B
,B
,B
,"B
-B
-�B
.B
-�B
-�B
.B
/B
/ B
/ B
/B
/5B
0!B
0B
0B
0;B
1'B
1'B
1B
2-B
2B
2B
2B
2-B
3B
33B
3B
49B
49B
4TB
5?B
5?B
6+B
6FB
6FB
6+B
7LB
7fB
8B
8RB
88B
88B
88B
8RB
9XB
9rB
9rB
:DB
:^B
;dB
;dB
;JB
<jB
<jB
<�B
=VB
=VB
=qB
=qB
=VB
=VB
=VB
=qB
=qB
=qB
>]B
>�B
>wB
?}B
?}B
?cB
@iB
@�B
A�B
A�B
AoB
BuB
B�B
C�B
C{B
C{B
C{B
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
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
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
NB
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
RB
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
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
VB
VB
V�B
W
B
V�B
W
B
X+B
X�B
X�B
X�B
X�B
X�B
ZB
ZB
ZB
ZB
[	B
[	B
[	B
[	B
[	B
\B
\)B
\B
\B
\B
]B
]B
]/B
]B
]B
]IB
^5B
^B
^B
_;B
_!B
_;B
_;B
`'B
`BB
`'B
`BB
`'B
`'B
`BB
`BB
`'B
a-B
aB
a-B
a-B
aB
aB
a-B
a-B
bB
b4B
b4B
b4B
bB
b4B
bNB
b4B
bNB
bNB
b4B
bNB
c:B
cTB
cTB
cTB
c:B
dZB
dZB
dZB
dZB
d@B
d@B
eFB
e`B
eFB
eFB
eFB
ffB
ffB
fLB
ffB
fLB
fLB
fLB
f2B
f2B
g8B
g8B
gRB
gmB
g8B
gmB
gRB
gRB
gRB
gRB
gRB
hXB
hsB
hsB
hXB
hsB
hXB
hXB
iDB
iyB
i_B
iyB
iDB
i_B
i_B
iyB
iyB
i_B
iDB
iDB
i_B
jeB
jeB
jB
jeB
jeB
jB
kQB
k�B
kkB
k�B
l�B
l�B
lqB
l�B
lqB
lqB
lqB
lWB
lWB
l�B
mwB
mwB
m�B
n}B
n}B
n}B
o�B
o�B
oiB
o�B
o�B
p�B
p�B
p�B
p�B
poB
p�B
poB
p�B
poB
poB
p�B
q�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</Q<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.31(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912290036532019122900365320191229003653202306231719442023062317194420230623171944201912300020412019123000204120191230002041  JA  ARFMdecpA19c                                                                20191224123718  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191224033719  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191224033722  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191224033722  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191224033723  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191224033723  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191224033723  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191224033723  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191224033723  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191224033723                      G�O�G�O�G�O�                JA  ARUP                                                                        20191224035342                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191224153535  CV  JULD            G�O�G�O�Fǭ�                JM  ARCAJMQC2.0                                                                 20191228153653  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191228153653  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191229152041  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081944  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                