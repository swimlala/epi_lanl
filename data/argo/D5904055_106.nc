CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-01-22T19:04:38Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue               conventions       Argo reference table 23          7�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    7�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >�E�vQ�        7�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    7�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        7�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9    PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9$   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9(   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9,   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  90   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  Lh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  o    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �p   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     ��   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     �         �Argo profile    3.1 1.2 19500101000000  20170122190438  20181103100346  5904055 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               jA   AO  5004                            2C  D   NAVIS_A                         863 @��[����1   @��\����@7]�E���d�~��"�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      jA   A   A   @�33@�  A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ Dؼ�D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�	�D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@w
=@�Q�@�Q�A(�A<(�A]A}A�{A�{A�{A�{A�{A�{A�{A�G�B
=B
=B
=B
=B'
=B/
=B7
=B?p�BG
=BO
=BW
=B_p�Bg
=Bo
=Bw
=B
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCC��CCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtp�Dt�Dup�Du�Dvp�Dv�Dwp�Dw�Dxp�Dx�Dyp�Dy�Dzp�Dz�D{p�D{�D|p�D|�D}p�D}�D~p�D~�Dp�D�D�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD���D��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD��RD��RD�8RD�xRD¸RD��RD�8RD�xRDøRD��RD�8RD�xRDĸRD��RD�8RD�xRDŸRD��RD�8RD�xRDƸRD��RD�8RD�xRDǸRD��RD�8RD�xRDȸRD��RD�8RD�xRDɸRD��RD�8RD�xRDʸRD��RD�8RD�xRD˸RD��RD�8RD�xRD̸RD��RD�8RD�xRD͸RD��RD�8RD�xRDθRD��RD�8RD�xRDϸRD��RD�8RD�xRDиRD��RD�8RD�xRDѸRD��RD�8RD�xRDҸRD��RD�8RD�xRDӸRD��RD�8RD�xRDԸRD��RD�8RD�xRDոRD��RD�8RD�xRDָRD��RD�8RD�xRD׸RD��RD�8RD�xRDصD��RD�8RD�xRDٸRD��RD�8RD�xRDڸRD��RD�8RD�xRD۸RD��RD�8RD�xRDܸRD��RD�8RD�xRDݸRD��RD�8RD�xRD޸RD��RD�8RD�xRD߸RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�D��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD�RD��RD�8RD�xRD��RD��RD�8RD�xRD���D��D�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AʋDAʧ�Aʥ�Aʥ�Aʥ�Aʧ�Aʣ�Aʣ�Aʡ�Aʣ�Aʧ�Aʩ�Aʩ�Aʰ!AʮAʬAʥ�Aʧ�AʬAʩ�Aʣ�Aʧ�Aʰ!AʶFAʲ-AʮAʰ!A�ȴA��yA�M�AɁA�&�AȬA���Aǧ�AǍPA�n�A�dZA�VA�7LA�{A�1A��HAƼjAƙ�A�1'A���AōPA�K�A��Aħ�A�\)A��A�z�A��9A�ZA�bA�{A���A���A�hsA��FA�"�A�ĜA�\)A��A���A��\A�p�A�ĜA��-A��!A��7A�VA�1'A��A�G�A��TA��`A���A���A��A���A�\)A��A��A�v�A�hsA���A��/A�A�M�A��A���A���A�r�A�Q�A���A�-A��\A���A�1'A�Q�A�ȴA�1A��A��mA��A�VA�ZA�JA��jA�7LA���A&�A|�/Az�Aw�^Av^5AuVAq�PAn��Am�Al��Al�jAj1'Ah��AfQ�Ad�Aa�mAa�PAa�A_VA\��AYdZAWS�AU�-AU�AT�jATQ�AS��AR�/AQ�#AL�AKp�AIt�AH{AF��AE�^ADbNA@�`A>�A=\)A:�\A6�\A5��A5��A5\)A5G�A5G�A4ĜA3;dA29XA1�
A1��A1p�A0�yA0~�A0�+A0�uA0��A0�!A0�yA0�A0�/A0��A0JA/33A.=qA-VA,��A,bNA+�FA+K�A*�A*-A)��A(9XA'K�A& �A$��A$r�A$JA"�RA �A�TAoAS�A�\A��A�PA=qA�AC�A��A��A��A=qA�
A�FAK�A~�A�A�AM�AA�PA=qA��A��A�A5?AC�A	��A�A$�AdZA+A��A�9A$�AC�AQ�A�Al�A �yA n�@���@�@��`@�@�$�@���@���@�r�@�Q�@���@�o@��@�{@���@��#@�`B@��@�Ĝ@�1@��@�@�1'@�"�@���@�dZ@��H@��@�7L@��
@���@ޗ�@ݡ�@�  @��#@�Q�@�^5@�1@ӍP@�K�@�"�@Ұ!@�`B@�dZ@��@͉7@�/@̼j@ʸR@�X@��/@ǶF@Ƈ+@�V@�{@Ų-@ēu@å�@��@�ff@��-@��`@��9@�bN@� �@���@��y@�@��h@��@���@���@�z�@�r�@��P@��#@��@�x�@�x�@�G�@��9@�C�@�
=@���@��+@�-@�`B@�I�@��F@�\)@�@��@��^@�x�@��@�Z@��P@���@�&�@� �@��!@�=q@���@���@���@��7@��h@�X@��9@��
@��@��@�X@��j@���@�Q�@�1@��
@�+@��@��\@��T@��@�%@��D@�Q�@��;@�t�@�"�@�@��y@���@���@�@�x�@���@���@��P@��@��@�-@�p�@�&�@�%@���@�(�@�(�@���@���@���@��-@���@�p�@�O�@�/@���@���@�bN@��@��@�~�@�v�@�^5@�5?@��@�j@�bN@�bN@�1@��R@���@��!@�=q@�?}@���@���@��u@�I�@�dZ@��R@�v�@�ff@�ff@�M�@�J@���@��7@�p�@�x�@�x�@��h@�@�@�$�@�-@�=q@�v�@���@�ff@��@���@��@�r�@�1'@��@��F@���@�33@��T@�G�@�&�@�%@���@��@��/@��j@���@��D@�I�@��@� �@�1'@�(�@�Q�@��9@��@��-@�5?@�n�@�v�@�~�@�n�@��@�G�@��@�Q�@�I�@�A�@�(�@��@��@�S�@�;d@�S�@�S�@�S�@�\)@�S�@�K�@�K�@�+@�o@��@��@�v�@���@���@�n�@�$�@�@��@��#@��^@���@�p�@��/@���@��@�Ĝ@�9X@�@
=@�@l�@+@~V@~5?@}��@}�@}/@}/@}�@|��@|I�@|�@{��@{ƨ@{��@{�@{S�@{33@{@z��@z�\@zM�@z�@y��@yX@yG�@y%@xr�@w�w@w\)@v�R@v��@v�+@u�T@u`B@u/@t�@t�D@t�@s��@sdZ@so@r�H@r^5@q��@q�^@p��@p1'@o�w@o|�@nv�@n{@m�@m��@m��@mO�@m?}@mV@l��@l�@l��@lZ@lz�@l��@k��@k�
@k�
@l�@l(�@l�@kƨ@k��@j�!@j�@jJ@i��@iX@hĜ@h�9@h�@hQ�@hb@hb@h  @g|�@g;d@f�y@fV@e�@d��@d�/@d�j@d�D@dj@dZ@d�@c�
@c��@ct�@b~�@a�@aX@`��@`��@`�u@`�@`r�@`A�@_�;@_l�@^��@^V@^E�@^{@]�-@]��@]�@]?}@\��@\�j@\�D@\I�@[�m@[S�@[@Z��@Z�\@Z^5@ZJ@Y�#@Y�^@Y��@Y�7@Y&�@Y%@X�`@XĜ@X�u@W�;@W�@W+@V�@Vȴ@V��@V$�@Up�@T��@S@R=q@RJ@Q��@Q�#@QG�@P��@P��@P�@PbN@O�@OK�@O�@N��@N�y@N��@N5?@N{@M�h@MO�@L��@L1@K"�@Jn�@J�\@J�@I�^@IG�@I&�@I�@H��@H��@H��@HĜ@H��@H�9@H��@Hr�@H �@G�;@G��@GK�@F�y@F$�@EO�@E/@E�@E/@E/@D��@D�/@D�@D(�@C��@CdZ@C"�@C"�@B�H@B^5@A�#@Ax�@@��@@bN@?�@?l�@?�@?
=@>�@>ȴ@>��@>��@>�R@>��@>E�@=�@=`B@=V@<��@<I�@;�m@;��@;S�@;@:��@:�!@:~�@:M�@:�@:J@9�@9��@9�7@9hs@9G�@9�@8��@8��@8�9@8  @6��@65?@5��@5V@4��@4z�@4j@4j@4Z@4I�@4I�@49X@4�@3��@3�m@3�F@3o@2J@1��@1��@1��@1��@1�7@1x�@1hs@1X@17L@0��@0Ĝ@0��@0�`@0�`@0r�@0r�@/|�@/�@.ȴ@.v�@.V@.5?@.{@-�@-��@-��@-�@,�D@,�@+��@+�
@+�
@+ƨ@+��@+dZ@*�@*�\@*�@)��@)��@)hs@)&�@(��@(  @'�;@'�w@'�P@'+@&��@&E�@&{@&@&@%��@%�@$�/@$��@$I�@#��@#��@#"�@"��@"n�@"=q@"-@"J@!�@!�@!�@!�@!��@!&�@ Ĝ@ �9@ �u@ A�@�@|�@\)@+@�@
=@
=@
=@
=@
=@�y@�+@ff@$�@�@�@�@p�@O�@O�@/@�/@�D@j@j@Z@9X@��@dZ@33@o@�@�@��@��@n�@=q@-@J@�@��@��@�@��@r�@ �@  @�;@�;@�;@�;@�;@�P@K�@�@�y@�@��@E�@��@�-@�h@O�@�/@��@Z@1@��@�m@�
@��@dZ@S�@�@��@n�@=q@��@�#@��@x�@hs@7L@%@�`@��@�u@r�@bN@A�@  @�;@�w@�@�P@\)@K�@;d@+@
=@�y@�R@�+@ff@E�@�-@�@p�@`B@�@�/@��@z�@j@I�@(�@1@�m@�
@��@t�@C�@o@@@
�H@
�!@
��@
=q@
J@	�^@	x�@	&�@�`@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AʋDAʧ�Aʥ�Aʥ�Aʥ�Aʧ�Aʣ�Aʣ�Aʡ�Aʣ�Aʧ�Aʩ�Aʩ�Aʰ!AʮAʬAʥ�Aʧ�AʬAʩ�Aʣ�Aʧ�Aʰ!AʶFAʲ-AʮAʰ!A�ȴA��yA�M�AɁA�&�AȬA���Aǧ�AǍPA�n�A�dZA�VA�7LA�{A�1A��HAƼjAƙ�A�1'A���AōPA�K�A��Aħ�A�\)A��A�z�A��9A�ZA�bA�{A���A���A�hsA��FA�"�A�ĜA�\)A��A���A��\A�p�A�ĜA��-A��!A��7A�VA�1'A��A�G�A��TA��`A���A���A��A���A�\)A��A��A�v�A�hsA���A��/A�A�M�A��A���A���A�r�A�Q�A���A�-A��\A���A�1'A�Q�A�ȴA�1A��A��mA��A�VA�ZA�JA��jA�7LA���A&�A|�/Az�Aw�^Av^5AuVAq�PAn��Am�Al��Al�jAj1'Ah��AfQ�Ad�Aa�mAa�PAa�A_VA\��AYdZAWS�AU�-AU�AT�jATQ�AS��AR�/AQ�#AL�AKp�AIt�AH{AF��AE�^ADbNA@�`A>�A=\)A:�\A6�\A5��A5��A5\)A5G�A5G�A4ĜA3;dA29XA1�
A1��A1p�A0�yA0~�A0�+A0�uA0��A0�!A0�yA0�A0�/A0��A0JA/33A.=qA-VA,��A,bNA+�FA+K�A*�A*-A)��A(9XA'K�A& �A$��A$r�A$JA"�RA �A�TAoAS�A�\A��A�PA=qA�AC�A��A��A��A=qA�
A�FAK�A~�A�A�AM�AA�PA=qA��A��A�A5?AC�A	��A�A$�AdZA+A��A�9A$�AC�AQ�A�Al�A �yA n�@���@�@��`@�@�$�@���@���@�r�@�Q�@���@�o@��@�{@���@��#@�`B@��@�Ĝ@�1@��@�@�1'@�"�@���@�dZ@��H@��@�7L@��
@���@ޗ�@ݡ�@�  @��#@�Q�@�^5@�1@ӍP@�K�@�"�@Ұ!@�`B@�dZ@��@͉7@�/@̼j@ʸR@�X@��/@ǶF@Ƈ+@�V@�{@Ų-@ēu@å�@��@�ff@��-@��`@��9@�bN@� �@���@��y@�@��h@��@���@���@�z�@�r�@��P@��#@��@�x�@�x�@�G�@��9@�C�@�
=@���@��+@�-@�`B@�I�@��F@�\)@�@��@��^@�x�@��@�Z@��P@���@�&�@� �@��!@�=q@���@���@���@��7@��h@�X@��9@��
@��@��@�X@��j@���@�Q�@�1@��
@�+@��@��\@��T@��@�%@��D@�Q�@��;@�t�@�"�@�@��y@���@���@�@�x�@���@���@��P@��@��@�-@�p�@�&�@�%@���@�(�@�(�@���@���@���@��-@���@�p�@�O�@�/@���@���@�bN@��@��@�~�@�v�@�^5@�5?@��@�j@�bN@�bN@�1@��R@���@��!@�=q@�?}@���@���@��u@�I�@�dZ@��R@�v�@�ff@�ff@�M�@�J@���@��7@�p�@�x�@�x�@��h@�@�@�$�@�-@�=q@�v�@���@�ff@��@���@��@�r�@�1'@��@��F@���@�33@��T@�G�@�&�@�%@���@��@��/@��j@���@��D@�I�@��@� �@�1'@�(�@�Q�@��9@��@��-@�5?@�n�@�v�@�~�@�n�@��@�G�@��@�Q�@�I�@�A�@�(�@��@��@�S�@�;d@�S�@�S�@�S�@�\)@�S�@�K�@�K�@�+@�o@��@��@�v�@���@���@�n�@�$�@�@��@��#@��^@���@�p�@��/@���@��@�Ĝ@�9X@�@
=@�@l�@+@~V@~5?@}��@}�@}/@}/@}�@|��@|I�@|�@{��@{ƨ@{��@{�@{S�@{33@{@z��@z�\@zM�@z�@y��@yX@yG�@y%@xr�@w�w@w\)@v�R@v��@v�+@u�T@u`B@u/@t�@t�D@t�@s��@sdZ@so@r�H@r^5@q��@q�^@p��@p1'@o�w@o|�@nv�@n{@m�@m��@m��@mO�@m?}@mV@l��@l�@l��@lZ@lz�@l��@k��@k�
@k�
@l�@l(�@l�@kƨ@k��@j�!@j�@jJ@i��@iX@hĜ@h�9@h�@hQ�@hb@hb@h  @g|�@g;d@f�y@fV@e�@d��@d�/@d�j@d�D@dj@dZ@d�@c�
@c��@ct�@b~�@a�@aX@`��@`��@`�u@`�@`r�@`A�@_�;@_l�@^��@^V@^E�@^{@]�-@]��@]�@]?}@\��@\�j@\�D@\I�@[�m@[S�@[@Z��@Z�\@Z^5@ZJ@Y�#@Y�^@Y��@Y�7@Y&�@Y%@X�`@XĜ@X�u@W�;@W�@W+@V�@Vȴ@V��@V$�@Up�@T��@S@R=q@RJ@Q��@Q�#@QG�@P��@P��@P�@PbN@O�@OK�@O�@N��@N�y@N��@N5?@N{@M�h@MO�@L��@L1@K"�@Jn�@J�\@J�@I�^@IG�@I&�@I�@H��@H��@H��@HĜ@H��@H�9@H��@Hr�@H �@G�;@G��@GK�@F�y@F$�@EO�@E/@E�@E/@E/@D��@D�/@D�@D(�@C��@CdZ@C"�@C"�@B�H@B^5@A�#@Ax�@@��@@bN@?�@?l�@?�@?
=@>�@>ȴ@>��@>��@>�R@>��@>E�@=�@=`B@=V@<��@<I�@;�m@;��@;S�@;@:��@:�!@:~�@:M�@:�@:J@9�@9��@9�7@9hs@9G�@9�@8��@8��@8�9@8  @6��@65?@5��@5V@4��@4z�@4j@4j@4Z@4I�@4I�@49X@4�@3��@3�m@3�F@3o@2J@1��@1��@1��@1��@1�7@1x�@1hs@1X@17L@0��@0Ĝ@0��@0�`@0�`@0r�@0r�@/|�@/�@.ȴ@.v�@.V@.5?@.{@-�@-��@-��@-�@,�D@,�@+��@+�
@+�
@+ƨ@+��@+dZ@*�@*�\@*�@)��@)��@)hs@)&�@(��@(  @'�;@'�w@'�P@'+@&��@&E�@&{@&@&@%��@%�@$�/@$��@$I�@#��@#��@#"�@"��@"n�@"=q@"-@"J@!�@!�@!�@!�@!��@!&�@ Ĝ@ �9@ �u@ A�@�@|�@\)@+@�@
=@
=@
=@
=@
=@�y@�+@ff@$�@�@�@�@p�@O�@O�@/@�/@�D@j@j@Z@9X@��@dZ@33@o@�@�@��@��@n�@=q@-@J@�@��@��@�@��@r�@ �@  @�;@�;@�;@�;@�;@�P@K�@�@�y@�@��@E�@��@�-@�h@O�@�/@��@Z@1@��@�m@�
@��@dZ@S�@�@��@n�@=q@��@�#@��@x�@hs@7L@%@�`@��@�u@r�@bN@A�@  @�;@�w@�@�P@\)@K�@;d@+@
=@�y@�R@�+@ff@E�@�-@�@p�@`B@�@�/@��@z�@j@I�@(�@1@�m@�
@��@t�@C�@o@@@
�H@
�!@
��@
=q@
J@	�^@	x�@	&�@�`@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BP�B�7B��B��B�B�^BɺB��B��B��B��B��B�B�
B�#B�;B�NB�sB�B�B�B��B
=B�B"�B{BDB��B��B��B�B�B�yB�sB�`B�NB�;B�/B�B�
B��B��B�}B�3B�3B�'B�B��B��B�PB�Br�BffBT�BF�B.B�BB�yB��B�!B��B�\BaHBI�B7LB33B0!B.B%�B!�B�B\B	7B
��B
��B
�B
�`B
�5B
�
B
��B
B
�jB
�FB
��B
�7B
z�B
gmB
S�B
B�B
7LB
,B
�B
B	��B	��B	��B	�sB	�/B	��B	ŢB	�FB	�-B	�B	��B	�oB	�B	u�B	m�B	iyB	gmB	e`B	aHB	\)B	R�B	=qB	5?B	,B	$�B	�B	�B	hB	B��B��B�yB�#B�
B�B��B��B��B��BĜB��B��B�}B�wB�qB�qBBĜBĜBŢBȴBɺBɺBȴBŢBB�wB�jB�dB�^B�LB�?B�'B�B�B�B��B��B��B��B��B��B�{B�oB�\B�PB�DB�=B�1B�%B�B�B�B�B�B~�B~�B~�B}�B}�B{�B{�B|�B{�Bx�Bt�Bs�Bq�Bn�Bl�BiyBgmBhsBhsBiyBjBjBjBjBk�Bk�Bm�Bp�Bq�Bp�Bo�Bn�Bn�Bo�Bn�Bp�Bo�Bp�Bo�Bn�Bm�Bp�Bs�Bs�Bs�Bt�Bt�Bu�Bv�Bw�Bz�Bz�Bz�Bz�Bz�By�Bz�By�Bx�Bw�Bv�Bu�Bq�Bo�Bo�Bp�Bt�Bu�Bv�Bv�Bu�Bx�B{�B|�B�B�B�1B�1B�7B�7B�JB�\B�\B�VB�JB�JB�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�'B�'B�-B�'B�!B�'B�-B�?B�XB�qBBŢBǮBɺBȴB��B��B��B��B��B��B��B�B�B�#B�)B�/B�;B�HB�TB�fB�sB�yB�B�B�B��B��B��B��B��B��B��B	B	B	B	+B		7B	
=B	JB	VB	\B	\B	\B	\B	VB	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	&�B	)�B	,B	1'B	5?B	9XB	<jB	=qB	=qB	>wB	>wB	>wB	?}B	@�B	A�B	B�B	G�B	L�B	Q�B	S�B	S�B	S�B	T�B	T�B	T�B	T�B	]/B	dZB	hsB	l�B	o�B	r�B	s�B	t�B	u�B	u�B	w�B	x�B	x�B	y�B	y�B	{�B	{�B	{�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�FB	�XB	�wB	B	ĜB	ĜB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�;B	�;B	�;B	�BB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B

=B
DB
JB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
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
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
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
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
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
6FB
5?B
6FB
7LB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
>wB
>wB
>wB
=qB
=qB
>wB
>wB
>wB
>wB
=qB
=qB
=qB
=qB
=qB
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
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
E�B
F�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
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
N�B
N�B
M�B
M�B
N�B
M�B
N�B
O�B
O�B
O�B
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
R�B
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
XB
XB
XB
XB
XB
YB
YB
YB
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
^5B
_;B
_;B
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
aHB
`BB
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
ffB
ffB
ffB
ffB
ffB
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
hsB
hsB
hsB
iyB
hsB
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
p�B
p�B
p�B
p�B
p�B
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
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BӉB�B�B�B��B�B�B�B��B��B��B�B��B�B�B�B��B��B�B�"B��B��B��B�#B�B�BӸB�aBS^B�~B�OB�B��B��B�:B�RB�B�.B�xBӔB�LB��B��B��B�B�6B�VB�[B��B��BB�B&`BuB�BOB�`B��B��B�(B�zB�	B�B�-B�\BޢB�5B״B֚B�B�5B��B�B��B�oB��B��B��B�yBw<Bn.BY�BPYB2�B"�B	�B��B֎B��B��B�mBh2BPB8jB3�B0�B10B'�B$�B#iByB\B�B
��B
�GB
�HB
�HB
�dB
�WB
�B
�B
�KB
��B
�dB
�B
nbB
Y�B
E�B
:�B
4�B
�B
�B	��B	�iB	��B	�cB	��B	�>B	��B	�@B	��B	�2B	��B	�SB	�+B	y�B	n�B	jFB	hDB	f�B	b�B	]�B	[�B	@LB	98B	/&B	(B	�B	�B	vB	
B�B��B��B��BלB�[B�DB�B�zB�B�UB¦B�+B�,B�B��B�yBBĤB�|B�-B��B��B�BʻB��B� B�cB� B��B�B�gB��B�jB��B�_B�RB��B��B�B��B�B��B��B��B�B�fB�GB�)B��B��B�dB��B�9B��B��B�B^B�B�BLB}�B}�B}�B}B{�Bx�Bu�Bs�Bo�Bn�Bm�Bm+Bj�Bj[Bj"BkBkWBk�Bl�BnBm�Bm�BrBsBrJBp�BqHBqBp�Bp!Bq7Bo�Bp�Bp4Bo�BqzBr�Bs�BtBtoBu<BuDBv�Bx�Bz�B{sB|VB}�B|�B{�B{7B|B{�B|�Bz�Bx,BxBtkBq�BrgBs�Bu~Bv<Bw#Bw�Bw�B{�B}�B}�B��B��B�&B�MB�B�B�/B��B��B�B�B��B��B�@B��B��B�B�>B�AB��B��B�B��B��B�ZB�?B�=B�B�+B�B��B�MB�EB�}B�B��B��B��B��B��B�rB��B�`B�1B�<B�B�9B�DB�jB��B�B�XB��B�zB� B��BۜBܓBݑB�tB�aB��B�B��B�B�#B��B��B�B�fB�XB�HB��B�PB��B	:B	�B	B	B		�B	B	
B	�B	�B	�B	�B	�B	oB	VB	�B	�B	B	�B	gB	<B	�B	@B	 B	'iB	+	B	,2B	23B	6�B	:iB	<�B	=�B	=�B	>�B	>�B	>�B	?�B	A[B	CB	C�B	H�B	L�B	R3B	TqB	U�B	U+B	U+B	U)B	U�B	W$B	]�B	d]B	i:B	m�B	pB	s B	t#B	u4B	v�B	v�B	x?B	y
B	x�B	zB	zFB	|VB	|PB	|%B	}B	~B	~�B	�B	��B	�B	�-B	�)B	��B	�B	��B	��B	��B	�B	�WB	�!B	� B	�B	��B	�tB	��B	��B	�%B	�%B	�B	�B	�B	�.B	�*B	�!B	�iB	�WB	�B	�B	�>B	��B	��B	��B	��B	��B	�UB	İB	İB	��B	ɢB	��B	��B	�mB	��B	�B	�B	�B	�<B	��B	�#B	��B	�B	�B	�B	�-B	�(B	�(B	�fB	�{B	ߎB	߆B	��B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	�zB	��B	�B	�B	�B	��B	�B	�B	�~B	�	B	�vB	��B	�$B	�!B	�%B	��B	��B	�.B	�GB	�B	�B	�!B	�B	�B	�+B	�&B	�3B	�>B
 GB
YB
WB
�B
bB
IB
pB
�B
�B
�B
�B
YB
iB
�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
-B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
~B
�B
�B
�B
�B
nB
B
�B
�B
.B
!B
�B
�B
�B
�B
�B
�B
,B
�B
B
MB
zB
6B
�B
�B
 B
�B
�B
 B
 B
 B
 B
 �B
!bB
!XB
!4B
!"B
 �B
 �B
!�B
"B
"?B
"MB
"�B
#&B
$B
$!B
$EB
%B
%B
&8B
&5B
':B
'-B
'5B
(NB
)qB
)EB
)<B
)/B
)0B
)DB
)1B
*0B
*&B
+9B
,cB
,;B
,;B
-CB
-OB
-�B
-OB
.�B
.fB
.=B
.VB
/�B
/�B
/�B
1AB
0�B
1eB
1QB
1^B
1�B
1{B
2cB
2�B
2eB
2�B
2�B
2mB
3jB
3\B
4�B
4�B
4qB
4�B
5�B
6�B
6�B
6�B
5�B
6RB
7�B
9�B
9�B
:�B
:�B
:�B
:�B
:B
:�B
:rB
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
<
B
<B
;�B
;�B
;xB
<�B
=�B
>�B
>�B
>�B
=�B
=�B
>�B
>�B
>�B
>�B
=�B
=�B
>B
=�B
=�B
@B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
@�B
@�B
BB
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
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
E�B
F�B
E�B
F0B
GkB
GJB
HB
HRB
IB
I�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
J�B
KB
KTB
K�B
MB
MB
L�B
L�B
L�B
L�B
NB
NB
N
B
N!B
NB
N�B
N�B
M�B
NDB
N�B
N�B
OGB
P<B
P<B
PB
PB
Q B
QB
QB
Q5B
QiB
Q~B
RfB
R"B
R B
SB
RB
S=B
S>B
SkB
SbB
TxB
TbB
TBB
TBB
TPB
T�B
U�B
V@B
VBB
VSB
V|B
V�B
W~B
WQB
X<B
X1B
X]B
XzB
X�B
YvB
Y�B
Y�B
Z�B
Z�B
[�B
[sB
[lB
[RB
[`B
[`B
[FB
[DB
[JB
[�B
[�B
\�B
\XB
\jB
\�B
]�B
]|B
^uB
^~B
^cB
^cB
_YB
_YB
_YB
_^B
_B
_�B
_{B
_�B
`�B
`�B
`aB
`qB
`~B
akB
`�B
a�B
a�B
a�B
ajB
awB
a�B
a�B
b�B
b�B
b�B
b�B
cwB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
e�B
f�B
f�B
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
h�B
h�B
h�B
i�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
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
p+B
p�B
p�B
p�B
p�B
p�B
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
s�B
s�B
s�B
s�B
s�B
tB
uB
uB
uB
uB
vB
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<L�W<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*�t<#�
<#�
<#�
</_.<#�
<#�
<#�
<#�
<#�
<#�
<#�
<`�!<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7�:<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.24 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310904232018103109042320181031090423  0303                            082713                          AO  ARCAADJP                                                                    20170122190438    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170122190438  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170122190438  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031090423  QC  PRES            @�33D�  G�O�                PM  ARSQCTM V1.1                                                                20181031090423  QC  PSAL            @�33D�  G�O�                PM  ARSQOWGUV1.0                                                                20181103100346  IP                  G�O�G�O�G�O�                