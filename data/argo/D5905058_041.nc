CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-21T18:36:00Z creation;2018-02-21T18:36:02Z conversion to V3.1;2019-12-23T06:26:37Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180221183600  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               )A   JA  I2_0675_041                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�N#���1   @�N$fff�@6�W>�6z�b�����D1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D��3D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�C3Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@G
=@��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�G�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\C5�CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC=DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[�qD\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~qD~��D�D��D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D�D���D�	�D�I�DÉ�D���D�	�D�I�Dĉ�D���D�	�D�I�Dŉ�D���D�	�D�I�DƉ�D���D�	�D�I�Dǉ�D���D�	�D�I�Dȉ�D���D�	�D�I�Dɉ�D���D�	�D�I�Dʉ�D���D�	�D�I�Dˉ�D���D�	�D�I�D̉�D���D�	�D�I�D͉�D���D�	�D�I�DΉ�D���D�	�D�I�Dω�D���D�	�D�I�DЉ�D���D�	�D�I�Dщ�D���D�	�D�I�D҉�D���D�	�D�I�DӉ�D���D�	�D�I�Dԉ�D��D�	�D�I�DՉ�D���D�	�D�I�D։�D���D�	�D�I�D׉�D���D�	�D�I�D؉�D���D�	�D�I�Dى�D���D�	�D�I�Dډ�D���D�	�D�MDۉ�D���D�	�D�I�D܉�D���D�	�D�I�D݉�D���D�	�D�I�Dމ�D���D�	�D�I�D߉�D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D���D���D�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A� �A� �A� �A�"�A� �A��A��A� �A�"�A�&�A�(�A�&�A�&�A�&�A�(�A�-A�/A�-A�/A�-A�1'A�1'A�33A�5?A�7LA�9XA�9XA�9XA�9XA�?}A�C�A�C�A�I�A�K�A�VA�9XA��;A�1'A��wA��DA���A�z�A�(�A�bA��wA�^5A��A�ĜA���A�I�A��^A���A�A�oA�?}A��-A��yA�O�A� �A�+A�VA�G�A��wA�G�A���A�+A��A�p�A�33A��A�{A���A��A�{A���A�C�A�p�A�O�A�oA��jA��A��A�"�A���A�C�A�ȴA���A�"�A�oA�"�A�E�A�?}A�oA��HA��A��;A�Q�A��A�1A��A�dZA���AO�A}��Az=qAy?}Axv�Aw\)Av��AuƨAs�TAr��Ap~�Al�\Ak
=Ah�AedZAc�#AbjA^-A\��A[�FAZ��AZ1AY��AY|�AY\)AX�HAW�
AUO�AS33AQ+AOt�ANr�AMS�AL��AL�AJ��AIAFz�AC�AA��A>A< �A;�FA9�mA7��A5��A3�^A2�HA0�A.^5A,��A+�
A(v�A%&�A#��A"�9A!�A!��A jA�A��A�HA�A�A�7Ar�Ax�A�A�#A`BAK�Ar�A��A;dA�A�\A�A
�`A
(�A	��A	�#AbAl�AȴA�7Av�AA�AoA��AA�A��A�FA/A �+A 5?@��w@�\)@�b@�C�@���@���@�E�@��@��@�x�@���@�@�1@�P@��H@�M�@���@�V@�b@�n�@�j@��#@�j@���@�F@��y@�V@�^@��/@�(�@��@�`B@�  @�@ݩ�@���@�"�@��@�&�@ם�@��@���@ԋD@Ӿw@��@��/@�\)@�{@̼j@�o@��@�hs@�b@��@�ff@�bN@�@��#@��@��9@��P@�X@���@�ȴ@��@��@�ƨ@�t�@�S�@�"�@��y@��j@��y@��@���@��!@�V@�J@���@�x�@��@�%@�V@���@�I�@�b@�t�@�;d@��@���@�j@�(�@���@��m@��
@��@�@��!@���@�5?@��@��-@�X@��@�9X@��@��@�o@�M�@���@��h@�x�@�p�@�X@�V@�Ĝ@��@�(�@��;@�;d@�^5@���@��7@�`B@�X@���@�Q�@�(�@��
@���@�l�@�ȴ@�ff@�5?@�=q@�$�@��@��-@��@��u@�j@�Z@�(�@�b@��;@���@�t�@�l�@�C�@�@��+@�$�@���@���@���@���@��7@��h@���@��@�G�@�/@�&�@���@��/@��j@���@�1'@���@��m@���@�\)@�;d@�S�@�;d@��@��R@���@�n�@�E�@�@���@�?}@�%@��/@��u@�9X@�1@�|�@�o@��H@��R@��\@�~�@�n�@�$�@���@�p�@�hs@��@��@�Ĝ@�z�@�9X@�  @��
@���@��F@�l�@�K�@�+@�"�@���@��H@���@��R@���@�~�@�M�@��@���@���@��@�O�@�&�@��/@��@��u@�1@��;@���@�\)@��@���@�ff@�=q@�{@�J@��T@��-@���@�p�@�`B@�O�@�&�@�%@��`@��u@�(�@��;@��F@��@�l�@�;d@��@�@��y@�ȴ@���@�^5@�=q@��@��@���@��^@��h@�x�@�hs@�G�@�&�@�%@���@���@��u@��u@�z�@��@���@���@�33@�@��@��@��H@���@���@��@�{@�{@�@��@���@��-@�p�@�/@��`@���@�I�@�b@�@�P@|�@K�@�@~��@~$�@}��@}@}�-@}p�@|�/@|�j@|�D@|j@|j@{�m@{��@{dZ@{C�@{C�@{C�@{S�@{o@z�H@zM�@zJ@y��@y�^@y��@yX@x��@x�@xQ�@x  @w�@w+@v��@vV@v5?@v$�@u�T@uO�@tz�@t�@s�F@sC�@r�H@r��@r��@r�!@r��@r^5@q&�@p�`@p�9@p��@p�@pQ�@o�;@o��@o;d@nv�@n@m��@mO�@l9X@j�@j^5@iX@h��@h�`@h��@g�@g\)@gK�@g+@f��@fff@e@e`B@d�/@d�j@dZ@d9X@cƨ@c�@b�@b�@a�#@ax�@a&�@`�`@`��@`1'@_��@^��@^��@^E�@]��@]O�@\�/@\��@\�D@\z�@\�@[�F@[��@[�@[C�@[C�@[33@[o@Z�!@Z~�@Zn�@Z=q@ZJ@Y��@Y��@Y��@Y��@Yhs@Y�@XĜ@X��@XQ�@X  @W�@W\)@W�@V��@V�y@VV@U`B@UV@T�@Tz�@T9X@T1@St�@So@R�!@Rn�@R^5@Q�#@QG�@Q�@Q�@P�`@PQ�@P1'@O�P@OK�@O;d@O+@N�@N{@M�@Mp�@L��@L�@L�D@LI�@K�F@KdZ@K33@J��@Jn�@JJ@I�7@I%@H��@HQ�@H1'@H  @G|�@G
=@F��@Fv�@F5?@E�T@E�-@E�@E?}@D��@D�j@DZ@Cƨ@CC�@Co@B�H@B��@B�!@B��@B^5@A�#@A&�@@��@@��@@Ĝ@@�9@@��@@�@@bN@@ �@?��@?\)@?;d@?+@>��@>��@>ff@>E�@>5?@>$�@>@=@=��@=O�@<�j@<I�@<9X@<1@;ƨ@;�@;dZ@;S�@;C�@:�@:��@:^5@:�@9�^@9x�@9hs@9X@9G�@8��@8�u@81'@7�w@7l�@6��@6��@65?@5��@5��@5?}@5V@4�j@4z�@4I�@4�@3��@3�@3"�@2�H@2��@2��@2�\@2~�@2n�@2-@1�@1�7@17L@0��@0�9@01'@/�;@/�w@/�P@/|�@/l�@/;d@/�@.�@.��@.�+@.�+@.ff@.5?@.@-�T@-�h@-O�@-?}@-�@-V@,��@,��@,�@,j@,I�@,9X@,�@,1@+��@+��@+dZ@+@*�!@*-@)�@)�#@)��@)x�@)G�@)&�@)%@(��@(��@(1'@'�;@'��@'�@'�P@'\)@';d@'+@'�@&�@&�R@&��@&ff@&V@&5?@&{@%�T@%��@%`B@%/@$��@$��@$��@$z�@$I�@#��@#��@#S�@#"�@"�!@"^5@"M�@!��@!��@!x�@!hs@!7L@!%@!%@!%@ �`@ �9@ �@ �@ �@ �@ �@ �@ �@ 1'@�@�P@;d@�@��@��@��@��@�y@�@�@ȴ@��@�T@��@`B@V@�@�j@��@z�@I�@�@�
@ƨ@��@C�@@��@��@�\@^5@��@��@x�@hs@7L@%@�`@Ĝ@�u@Q�@��@�@�P@|�@l�@l�@K�@K�@�@�R@ff@{@�@�-@p�@O�@?}@�@�@�j@z�@��@�F@��@33@�@��@�\@~�@^5@��@��@%@�9@�u@A�@1'@1'@�w@�P@\)@;d@
=@ȴ@��@ff@$�@@�T@@�h@`B@?}@�/@�@�@j@�m@ƨ@�
@�
@ƨ@��@��@�@�@t�@S�@"�@@
�\@
^5@
-@	�@	�#@	�7@	X@	G�@	7L@	&�@	%@�9@�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A� �A� �A� �A�"�A� �A��A��A� �A�"�A�&�A�(�A�&�A�&�A�&�A�(�A�-A�/A�-A�/A�-A�1'A�1'A�33A�5?A�7LA�9XA�9XA�9XA�9XA�?}A�C�A�C�A�I�A�K�A�VA�9XA��;A�1'A��wA��DA���A�z�A�(�A�bA��wA�^5A��A�ĜA���A�I�A��^A���A�A�oA�?}A��-A��yA�O�A� �A�+A�VA�G�A��wA�G�A���A�+A��A�p�A�33A��A�{A���A��A�{A���A�C�A�p�A�O�A�oA��jA��A��A�"�A���A�C�A�ȴA���A�"�A�oA�"�A�E�A�?}A�oA��HA��A��;A�Q�A��A�1A��A�dZA���AO�A}��Az=qAy?}Axv�Aw\)Av��AuƨAs�TAr��Ap~�Al�\Ak
=Ah�AedZAc�#AbjA^-A\��A[�FAZ��AZ1AY��AY|�AY\)AX�HAW�
AUO�AS33AQ+AOt�ANr�AMS�AL��AL�AJ��AIAFz�AC�AA��A>A< �A;�FA9�mA7��A5��A3�^A2�HA0�A.^5A,��A+�
A(v�A%&�A#��A"�9A!�A!��A jA�A��A�HA�A�A�7Ar�Ax�A�A�#A`BAK�Ar�A��A;dA�A�\A�A
�`A
(�A	��A	�#AbAl�AȴA�7Av�AA�AoA��AA�A��A�FA/A �+A 5?@��w@�\)@�b@�C�@���@���@�E�@��@��@�x�@���@�@�1@�P@��H@�M�@���@�V@�b@�n�@�j@��#@�j@���@�F@��y@�V@�^@��/@�(�@��@�`B@�  @�@ݩ�@���@�"�@��@�&�@ם�@��@���@ԋD@Ӿw@��@��/@�\)@�{@̼j@�o@��@�hs@�b@��@�ff@�bN@�@��#@��@��9@��P@�X@���@�ȴ@��@��@�ƨ@�t�@�S�@�"�@��y@��j@��y@��@���@��!@�V@�J@���@�x�@��@�%@�V@���@�I�@�b@�t�@�;d@��@���@�j@�(�@���@��m@��
@��@�@��!@���@�5?@��@��-@�X@��@�9X@��@��@�o@�M�@���@��h@�x�@�p�@�X@�V@�Ĝ@��@�(�@��;@�;d@�^5@���@��7@�`B@�X@���@�Q�@�(�@��
@���@�l�@�ȴ@�ff@�5?@�=q@�$�@��@��-@��@��u@�j@�Z@�(�@�b@��;@���@�t�@�l�@�C�@�@��+@�$�@���@���@���@���@��7@��h@���@��@�G�@�/@�&�@���@��/@��j@���@�1'@���@��m@���@�\)@�;d@�S�@�;d@��@��R@���@�n�@�E�@�@���@�?}@�%@��/@��u@�9X@�1@�|�@�o@��H@��R@��\@�~�@�n�@�$�@���@�p�@�hs@��@��@�Ĝ@�z�@�9X@�  @��
@���@��F@�l�@�K�@�+@�"�@���@��H@���@��R@���@�~�@�M�@��@���@���@��@�O�@�&�@��/@��@��u@�1@��;@���@�\)@��@���@�ff@�=q@�{@�J@��T@��-@���@�p�@�`B@�O�@�&�@�%@��`@��u@�(�@��;@��F@��@�l�@�;d@��@�@��y@�ȴ@���@�^5@�=q@��@��@���@��^@��h@�x�@�hs@�G�@�&�@�%@���@���@��u@��u@�z�@��@���@���@�33@�@��@��@��H@���@���@��@�{@�{@�@��@���@��-@�p�@�/@��`@���@�I�@�b@�@�P@|�@K�@�@~��@~$�@}��@}@}�-@}p�@|�/@|�j@|�D@|j@|j@{�m@{��@{dZ@{C�@{C�@{C�@{S�@{o@z�H@zM�@zJ@y��@y�^@y��@yX@x��@x�@xQ�@x  @w�@w+@v��@vV@v5?@v$�@u�T@uO�@tz�@t�@s�F@sC�@r�H@r��@r��@r�!@r��@r^5@q&�@p�`@p�9@p��@p�@pQ�@o�;@o��@o;d@nv�@n@m��@mO�@l9X@j�@j^5@iX@h��@h�`@h��@g�@g\)@gK�@g+@f��@fff@e@e`B@d�/@d�j@dZ@d9X@cƨ@c�@b�@b�@a�#@ax�@a&�@`�`@`��@`1'@_��@^��@^��@^E�@]��@]O�@\�/@\��@\�D@\z�@\�@[�F@[��@[�@[C�@[C�@[33@[o@Z�!@Z~�@Zn�@Z=q@ZJ@Y��@Y��@Y��@Y��@Yhs@Y�@XĜ@X��@XQ�@X  @W�@W\)@W�@V��@V�y@VV@U`B@UV@T�@Tz�@T9X@T1@St�@So@R�!@Rn�@R^5@Q�#@QG�@Q�@Q�@P�`@PQ�@P1'@O�P@OK�@O;d@O+@N�@N{@M�@Mp�@L��@L�@L�D@LI�@K�F@KdZ@K33@J��@Jn�@JJ@I�7@I%@H��@HQ�@H1'@H  @G|�@G
=@F��@Fv�@F5?@E�T@E�-@E�@E?}@D��@D�j@DZ@Cƨ@CC�@Co@B�H@B��@B�!@B��@B^5@A�#@A&�@@��@@��@@Ĝ@@�9@@��@@�@@bN@@ �@?��@?\)@?;d@?+@>��@>��@>ff@>E�@>5?@>$�@>@=@=��@=O�@<�j@<I�@<9X@<1@;ƨ@;�@;dZ@;S�@;C�@:�@:��@:^5@:�@9�^@9x�@9hs@9X@9G�@8��@8�u@81'@7�w@7l�@6��@6��@65?@5��@5��@5?}@5V@4�j@4z�@4I�@4�@3��@3�@3"�@2�H@2��@2��@2�\@2~�@2n�@2-@1�@1�7@17L@0��@0�9@01'@/�;@/�w@/�P@/|�@/l�@/;d@/�@.�@.��@.�+@.�+@.ff@.5?@.@-�T@-�h@-O�@-?}@-�@-V@,��@,��@,�@,j@,I�@,9X@,�@,1@+��@+��@+dZ@+@*�!@*-@)�@)�#@)��@)x�@)G�@)&�@)%@(��@(��@(1'@'�;@'��@'�@'�P@'\)@';d@'+@'�@&�@&�R@&��@&ff@&V@&5?@&{@%�T@%��@%`B@%/@$��@$��@$��@$z�@$I�@#��@#��@#S�@#"�@"�!@"^5@"M�@!��@!��@!x�@!hs@!7L@!%@!%@!%@ �`@ �9@ �@ �@ �@ �@ �@ �@ �@ 1'@�@�P@;d@�@��@��@��@��@�y@�@�@ȴ@��@�T@��@`B@V@�@�j@��@z�@I�@�@�
@ƨ@��@C�@@��@��@�\@^5@��@��@x�@hs@7L@%@�`@Ĝ@�u@Q�@��@�@�P@|�@l�@l�@K�@K�@�@�R@ff@{@�@�-@p�@O�@?}@�@�@�j@z�@��@�F@��@33@�@��@�\@~�@^5@��@��@%@�9@�u@A�@1'@1'@�w@�P@\)@;d@
=@ȴ@��@ff@$�@@�T@@�h@`B@?}@�/@�@�@j@�m@ƨ@�
@�
@ƨ@��@��@�@�@t�@S�@"�@@
�\@
^5@
-@	�@	�#@	�7@	X@	G�@	7L@	&�@	%@�9@�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BǮBǮBǮBǮBǮBǮBǮBƨBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBǮBȴBɺBȴBȴBɺB��B��B��B��B�B�B�ZB��BDB,B=qBH�BR�BS�BR�BS�BYB[#BQ�BJ�BH�BH�BN�BaHBl�BhsBbNBXBL�BM�BO�BK�BE�B>wB<jB;dB9XB0!B"�B�BuB	7BBB��B�B�TB��B��B��BǮBĜB�3B��B�Bp�B49B  B
�B
�ZB
�B
��B
ŢB
�RB
��B
��B
�uB
�B
r�B
l�B
k�B
hsB
`BB
E�B
/B
(�B
oB
	7B
B	��B	��B	�B	�;B	�B	��B	�B	��B	�bB	s�B	bNB	YB	?}B	49B	.B	&�B	#�B	"�B	!�B	!�B	�B	�B	PB	B��B�B�mB�ZB�;B�)B�B�
B��B�dB��B��B�bB�PB�7B�B{�Bv�Bq�Bt�Bn�BcTB\)B[#BQ�BM�BL�BI�BG�BG�BC�B@�BB�B@�B=qB<jB:^B9XB5?B-B,B%�B$�B$�B'�B)�B)�B+B,B)�B(�B'�B)�B%�B%�B%�B$�B#�B#�B"�B"�B"�B"�B!�B#�B#�B"�B#�B"�B'�B%�B%�B%�B%�B(�B'�B)�B)�B(�B)�B)�B)�B)�B)�B(�B(�B(�B+B+B,B+B+B,B+B+B+B+B,B,B-B.B-B.B-B.B/B0!B1'B1'B1'B2-B2-B2-B33B49B6FB7LB9XB9XB;dB<jB;dB>wB>wB?}B?}B?}BA�BE�BG�BH�BL�BP�BYB^5B`BBbNBcTBl�Bs�Bt�Bt�Bu�Bw�By�B{�B~�B�B�%B�=B�hB�hB�oB��B��B��B��B��B�B�B�B�B�B�3B�?B�LB�jB�jB�jB�qB��BBŢBɺB��B��B�B�B�
B�B�B�B�)B�5B�;B�;B�`B�sB�B�B�B�B��B��B��B	  B	B	B		7B	VB	oB	oB	uB	uB	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	&�B	(�B	-B	33B	7LB	9XB	=qB	@�B	A�B	C�B	E�B	F�B	G�B	K�B	L�B	M�B	O�B	Q�B	R�B	T�B	XB	XB	[#B	_;B	`BB	gmB	l�B	m�B	r�B	s�B	t�B	u�B	v�B	x�B	y�B	|�B	~�B	�B	�B	�7B	�=B	�VB	�\B	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�FB	�LB	�LB	�RB	�XB	�dB	�jB	�qB	�wB	��B	��B	B	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
JB
JB
JB
PB
PB
PB
PB
PB
PB
VB
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
"�B
!�B
"�B
"�B
#�B
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
%�B
%�B
%�B
%�B
%�B
&�B
&�B
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
.B
.B
.B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
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
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
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
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
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
L�B
L�B
L�B
L�B
M�B
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
O�B
O�B
O�B
O�B
O�B
P�B
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
\)B
\)B
]/B
\)B
\)B
]/B
]/B
\)B
]/B
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
bNB
bNB
bNB
bNB
cTB
cTB
cTB
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
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
k�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BǔBǔBǔBǔBǔBǔBǔBƎBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBǔBȚBɠBȚBȚBɠBʦBοB��B��B��B�B�@B��B)B+�B=VBH�BR�BS�BR�BS�BX�B[	BQ�BJ�BH�BH�BN�Ba-BlqBhXBb4BW�BL�BM�BO�BK�BE�B>]B<PB;JB9>B0B"�BB[B	B�BB��B�B�:B��BˬBʦBǔBāB�B�kB��Bp�B4B
��B
�B
�@B
��B
ѷB
ňB
�8B
��B
�yB
�[B
��B
r�B
lqB
kkB
hXB
`'B
EmB
/ B
(�B
TB
	B
B	��B	��B	��B	�!B	��B	ʌB	��B	��B	�.B	s�B	b4B	X�B	?cB	4B	-�B	&�B	#�B	"�B	!�B	!�B	�B	B	6B	 �B��B�wB�RB�&B�!B�B��B��B͹B�0B��B��B�HB�6B�B��B{�Bv�Bq�Bt�Bn}Bc:B\B[	BQ�BM�BL�BI�BG�BGzBC{B@iBBuB@iB=<B<PB:*B9>B5%B,�B+�B%�B$�B$�B'�B)�B)�B*�B+�B)�B(�B'�B)�B%�B%�B%�B$�B#�B#�B"�B"�B"�B"�B!�B#�B#�B"�B#�B"�B'�B%�B%�B%�B%�B(�B'�B)�B)�B(�B)�B)�B)�B)�B)�B(�B(�B(�B*�B*�B+�B*�B*�B+�B*�B*�B*�B*�B+�B+�B,�B-�B,�B-�B,�B-�B.�B0B0�B1B1B1�B2B2B3B4B6+B72B9$B9>B;JB<PB;0B>]B>BB?HB?cB?HBAoBEmBG�BH�BL�BP�BX�B^B`BbBc BlWBs�Bt�Bt�Bu�Bw�By�B{�B~�B��B��B�#B�NB�4B�TB�gB�mB�_B��B��B��B��B��B��B��B�B�B�2B�6B�PB�6B�<B�OB�[B�mBɆBˬB��B��B��B��B��B��B��B��B�B�B�!B�,B�XB�]B�oB�vB�B��B��B��B��B	�B	�B		B	<B	TB	:B	@B	[B	@B	SB	B	qB	qB	~B	�B	!�B	#�B	$�B	&�B	(�B	,�B	3B	72B	9$B	=<B	@OB	AoB	CaB	EmB	FtB	G�B	K�B	L�B	M�B	O�B	Q�B	R�B	T�B	W�B	W�B	Z�B	_B	`'B	g8B	lqB	m]B	r|B	s�B	t�B	u�B	v�B	x�B	y�B	|�B	~�B	��B	��B	�B	�	B	�"B	�(B	�4B	�TB	�[B	�FB	�FB	�SB	��B	��B	�~B	��B	��B	��B	��B	��B	��B	� B	� B	� B	�B	�B	��B	��B	��B	�B	�B	�B	�+B	�B	�2B	�8B	�$B	�JB	�6B	�<B	�BB	�OB	�UB	�uB	āB	�mB	�mB	ǔB	ɆB	˒B	͟B	οB	��B	ϫB	бB	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�!B	�'B	�B	�B	�B	�:B	� B	�FB	�FB	�LB	�RB	�>B	�>B	�_B	�_B	�KB	�KB	�QB	�qB	�wB	�]B	�}B	�wB	�cB	�iB	�oB	��B	�|B	�B	�B	�|B	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
B
B
�B
�B
B
B
B
B
�B
�B
B
B
	B
	B
	B
	B

	B
)B
0B
B
0B
6B
B
B
6B
B
B
<B
BB
(B
.B
.B
.B
.B
4B
4B
NB
TB
[B
[B
[B
aB
MB
MB
YB
sB
YB
YB
_B
yB
_B
_B
eB
B
B
�B
�B
�B
�B
qB
qB
qB
xB
�B
xB
~B
~B
�B
�B
�B
~B
�B
�B
�B
 �B
 �B
!�B
!�B
"�B
!�B
"�B
"�B
#�B
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
%�B
%�B
%�B
%�B
%�B
&�B
&�B
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
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
/ B
/�B
0B
/�B
0B
0B
/�B
1B
2B
2B
1�B
2B
2B
3B
4B
4B
4B
5B
5%B
5%B
6+B
6B
6+B
6+B
6B
7B
7B
7B
7B
8B
8B
8B
9>B
9>B
9$B
9>B
:DB
:DB
:DB
:DB
:DB
:*B
:DB
;0B
;0B
<PB
<PB
<6B
<6B
<PB
<6B
<PB
<PB
<6B
=<B
=VB
=VB
=<B
=<B
>BB
>BB
>]B
>]B
>BB
>]B
?HB
?HB
?cB
@OB
@OB
@iB
@iB
AUB
AoB
AoB
AUB
AoB
AUB
BuB
B[B
B[B
B[B
C{B
CaB
C{B
CaB
CaB
C{B
D�B
DgB
DgB
EmB
EmB
FtB
F�B
F�B
G�B
G�B
GzB
G�B
G�B
H�B
H�B
H�B
I�B
I�B
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
L�B
L�B
L�B
L�B
M�B
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
O�B
O�B
O�B
O�B
O�B
P�B
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
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
ZB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[	B
[	B
[	B
Z�B
Z�B
Z�B
[	B
[�B
[�B
\B
[�B
[�B
[�B
]B
\B
[�B
]B
]B
[�B
\�B
^B
^B
^B
^B
^B
^B
^B
_!B
_!B
_!B
_!B
_B
_B
`B
`B
`B
`B
`'B
`'B
a-B
a-B
aB
aB
aB
aB
bB
b4B
b4B
bB
c:B
c B
c B
c B
c:B
c B
c:B
c:B
c:B
d@B
d&B
d@B
d&B
eFB
e,B
eFB
e,B
eFB
fLB
fLB
f2B
f2B
gRB
gRB
g8B
gRB
hXB
hXB
h>B
hXB
hXB
i_B
jKB
jeB
jeB
kQB
jKB
jeB
kkB
kkB
kkB
kQB
lWB
lqB
lqB
lqB
m]B
mwB
m]B
m]B
mwB
mwB
n}B
n}B
ncB
n}B
n}B
oiB
o�B
o�B
o�B
oiB
o�B
o�B
o�B
o�B
o�B
p�B
p�B
poB
poB
poB
qvB
q�B
qvB
q�B
r�B
r�B
r|B
r�B
r|B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.31(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802270104272018022701042720180227010427201804060310562018040603105620180406031056JA  ARFMdecpA19c                                                                20180222033522  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180221183600  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180221183600  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180221183601  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180221183602  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180221183602  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180221183602  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180221183602  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180221183602  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180221183602                      G�O�G�O�G�O�                JA  ARUP                                                                        20180221185615                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180222153917  CV  JULD            G�O�G�O�F�q                JM  ARCAJMQC2.0                                                                 20180226160427  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180226160427  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405181056  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                