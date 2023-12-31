CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:50Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
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
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190550  20181005190550  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�1Ù1   @��*ff{`@1��l�C��c����l�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C�C�C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��C��C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� DfD� D  D� D  D� D  D� D  D� D  D� DfD�fD	  D	y�D
  D
� D  Dy�D  D� D  D� D  D�fDfD� D��D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� DfD�fD  D� D  D� D  D� D��Dy�D��Dy�D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$fD$�fD%  D%� D&  D&y�D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0y�D1  D1�fD2fD2� D2��D3� D4  D4�fD5  D5y�D5��D6y�D6��D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DF��DGy�DH  DH� DIfDI� DI��DJy�DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDUfDU�fDVfDV�fDW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[y�D[��D\� D]  D]� D^  D^� D^��D_y�D`  D`� D`��Da� Db  Db� DcfDc�fDd  Dd�fDefDe�fDffDf�fDg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dj��Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Dsy�Dt  Dt� Dt��Du� DvfDv� Dw  Dw� Dw��Dy�fD�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A���A�(�A�(�A�(�A�(�A�(�A�(�B{B	z�B{B{B!{B){B1{B9z�BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B�W
B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=BؽpB܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECEC^�CEC
ECECECEC^�C^�CECECECECEC ^�C"EC$EC&EC(EC*EC,EC.+�C0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECp+�Cr+�CtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C��C��C��C��C��C�"�C�"�C�/\C�/\C��C��C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HD
�D�HD�D�HDHD�HDHD�HDHD�HDHD�HDHD�HD�D��D	HD	��D
HD
�HDHD��DHD�HDHD�HDHD��D�D�HD
�D�HDHD�HDHD�HDHD�HDHD�HD�D�HDHD�HDHD�HD�D��DHD�HDHD�HDHD�HD
�D��D
�D��DHD�HDHD�HD 
�D �HD!HD!�HD"HD"�HD#HD#�HD$�D$��D%HD%�HD&HD&��D'HD'�HD(HD(�HD)HD)�HD*
�D*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0
�D0��D1HD1��D2�D2�HD3
�D3�HD4HD4��D5HD5��D6
�D6��D7
�D7�HD8HD8�HD9HD9�HD:HD:�HD;�D;�HD<HD<�HD=
�D=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDF
�DF�HDG
�DG��DHHDH�HDI�DI�HDJ
�DJ��DKHDK�HDLHDL�HDMHDM�HDN
�DN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT��DU�DU��DV�DV��DWHDW��DXHDX�HDYHDY�HDZHDZ�HD[HD[��D\
�D\�HD]HD]�HD^HD^�HD_
�D_��D`HD`�HDa
�Da�HDbHDb�HDc�Dc��DdHDd��De�De��Df�Df��DgHDg��DhHDh�HDiHDi�HDjHDj�HDk
�Dk��DlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq��DrHDr�HDsHDs��DtHDt�HDu
�Du�HDv�Dv�HDwHDw�HDw�Dy��D�R�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�A�v�A�v�A�x�A�t�A�t�A�v�A�x�A�x�A�z�A�~�Aȇ+AȓuAȑhAȉ7A�\)A���AǇ+A�/A���A��A�-A�JA��TA�A�t�A�^5A�dZA�^5A�XA�^5A�Q�A�$�A��A�bA�  A�oA�+A�I�A�VA�p�A�Q�A�$�A���A�t�AœuA��mA�A�bA���Aŧ�A�t�A�+A��A�z�AÍPA���A�z�A�dZA�?}A��A��A��yA�;dA�hsA�p�A��A�ffA�A�A�O�A��^A��A�bNA��jA���A��wA�~�A���A�JA��!A�O�A���A���A�+A�\)A���A��TA��A�$�A��A��TA��A�jA��-A��`A���A�A�A��7A��A��FA�C�A�ĜA��mA�x�A��A��PA�"�A��FA���A���A��A�VA���A~{AzĜAv�Ar��Ao��AiO�Ac��A`�AWO�AQ�FAO7LAM�7AK�hAI��AF�AD�jACS�ABZA>M�A<�A<{A:v�A8�uA7�A6A�A4�+A3��A2�HA1�A0��A/G�A.n�A-�FA-+A+�A*JA)%A(9XA'XA%�^A$��A#dZA"E�A �uA��A�+A|�A�A~�A�wA��A��AQ�A  A��A��AS�A��AjAJAx�A�AAO�A�A�+AJAA�yA&�AZA�!A�-A
�/A
��A
bNA	��A5?Ap�AȴA�A+AI�A��A��AJA7LA^5A��A �HA �@���@�-@�/@���@�^5@��@���@�r�@� �@�"�@�&�@���@��H@�+@�@�h@�&�@�%@�j@�1@���@�@�D@�(�@�R@�p�@�Q�@���@�-@��T@�`B@���@���@�bN@� �@��@�|�@��@�V@��#@݉7@�z�@۝�@�;d@��@ڗ�@�=q@ى7@��
@׮@֏\@�p�@ԣ�@�I�@� �@��m@Ӯ@�\)@���@�&�@���@��/@Ь@�I�@��;@��@�-@�p�@�r�@ʟ�@��@�?}@��`@��/@ȼj@�Z@� �@���@Ǯ@�\)@�5?@őh@��@��`@Ĭ@��@Õ�@å�@�1@��m@�dZ@�K�@�5?@���@��#@��@�X@��@�(�@�K�@��H@�=q@�@��h@�7L@��9@�1@��;@��P@�"�@��@���@��+@�{@��@��@��@�1'@���@�\)@���@�J@��^@��@��7@��h@��h@��h@�7L@���@��9@���@��@�z�@� �@�ƨ@�C�@���@�~�@�v�@�n�@�M�@���@�x�@�G�@��@���@��m@���@�$�@��#@���@�hs@�p�@��7@��T@��h@�x�@�X@�/@��j@��D@�1@���@�C�@��R@�@�O�@��@��w@�l�@�"�@�^5@��#@��@�?}@���@�r�@��
@�dZ@���@��R@��!@�n�@��@���@��@��@���@�n�@�-@���@��-@�`B@�G�@���@��`@���@��u@�A�@��m@��P@�+@���@���@��\@�~�@�~�@�v�@�V@��#@�`B@�7L@��@��9@��u@��@�z�@�bN@�1@���@�\)@�o@���@��y@��H@��@��!@�ff@�^5@�-@���@�x�@�p�@�hs@�`B@�X@�O�@�G�@�G�@�G�@��@���@���@�A�@��@��
@��F@���@��P@�l�@�33@��H@�ȴ@��R@���@�M�@�5?@�5?@��@��@�@���@�O�@�/@�&�@��@��@�V@���@��@��/@��D@�Q�@�(�@��@��
@��F@���@��P@��@�dZ@�33@��@�v�@��-@���@���@�p�@�`B@�G�@���@~�}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�t�A�v�A�v�A�x�A�t�A�t�A�v�A�x�A�x�A�z�A�~�Aȇ+AȓuAȑhAȉ7A�\)A���AǇ+A�/A���A��A�-A�JA��TA�A�t�A�^5A�dZA�^5A�XA�^5A�Q�A�$�A��A�bA�  A�oA�+A�I�A�VA�p�A�Q�A�$�A���A�t�AœuA��mA�A�bA���Aŧ�A�t�A�+A��A�z�AÍPA���A�z�A�dZA�?}A��A��A��yA�;dA�hsA�p�A��A�ffA�A�A�O�A��^A��A�bNA��jA���A��wA�~�A���A�JA��!A�O�A���A���A�+A�\)A���A��TA��A�$�A��A��TA��A�jA��-A��`A���A�A�A��7A��A��FA�C�A�ĜA��mA�x�A��A��PA�"�A��FA���A���A��A�VA���A~{AzĜAv�Ar��Ao��AiO�Ac��A`�AWO�AQ�FAO7LAM�7AK�hAI��AF�AD�jACS�ABZA>M�A<�A<{A:v�A8�uA7�A6A�A4�+A3��A2�HA1�A0��A/G�A.n�A-�FA-+A+�A*JA)%A(9XA'XA%�^A$��A#dZA"E�A �uA��A�+A|�A�A~�A�wA��A��AQ�A  A��A��AS�A��AjAJAx�A�AAO�A�A�+AJAA�yA&�AZA�!A�-A
�/A
��A
bNA	��A5?Ap�AȴA�A+AI�A��A��AJA7LA^5A��A �HA �@���@�-@�/@���@�^5@��@���@�r�@� �@�"�@�&�@���@��H@�+@�@�h@�&�@�%@�j@�1@���@�@�D@�(�@�R@�p�@�Q�@���@�-@��T@�`B@���@���@�bN@� �@��@�|�@��@�V@��#@݉7@�z�@۝�@�;d@��@ڗ�@�=q@ى7@��
@׮@֏\@�p�@ԣ�@�I�@� �@��m@Ӯ@�\)@���@�&�@���@��/@Ь@�I�@��;@��@�-@�p�@�r�@ʟ�@��@�?}@��`@��/@ȼj@�Z@� �@���@Ǯ@�\)@�5?@őh@��@��`@Ĭ@��@Õ�@å�@�1@��m@�dZ@�K�@�5?@���@��#@��@�X@��@�(�@�K�@��H@�=q@�@��h@�7L@��9@�1@��;@��P@�"�@��@���@��+@�{@��@��@��@�1'@���@�\)@���@�J@��^@��@��7@��h@��h@��h@�7L@���@��9@���@��@�z�@� �@�ƨ@�C�@���@�~�@�v�@�n�@�M�@���@�x�@�G�@��@���@��m@���@�$�@��#@���@�hs@�p�@��7@��T@��h@�x�@�X@�/@��j@��D@�1@���@�C�@��R@�@�O�@��@��w@�l�@�"�@�^5@��#@��@�?}@���@�r�@��
@�dZ@���@��R@��!@�n�@��@���@��@��@���@�n�@�-@���@��-@�`B@�G�@���@��`@���@��u@�A�@��m@��P@�+@���@���@��\@�~�@�~�@�v�@�V@��#@�`B@�7L@��@��9@��u@��@�z�@�bN@�1@���@�\)@�o@���@��y@��H@��@��!@�ff@�^5@�-@���@�x�@�p�@�hs@�`B@�X@�O�@�G�@�G�@�G�@��@���@���@�A�@��@��
@��F@���@��P@�l�@�33@��H@�ȴ@��R@���@�M�@�5?@�5?@��@��@�@���@�O�@�/@�&�@��@��@�V@���@��@��/@��D@�Q�@�(�@��@��
@��F@���@��P@��@�dZ@�33@��@�v�@��-@���@���@�p�@�`B@�G�@���@~�}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�)B�)B�)B�)B�)B�)B�)B�/B�/B�/B�5B�HB�mB�B��B	{B	G�B	t�B	��B	��B	��B	�/B	�B	��B
	7B
uB
�B
&�B
+B
/B
33B
6FB
:^B
I�B
ZB
jB
�B
��B
ɺB
�B
�sB
�B
�B
�B
�B+B'�B0!B5?B@�BVB\)B^5BffBv�B�hB�'B�qB��B��B�B�HB�B��B�B�ZB�B��B  BB+B
=B	7B+B%BBB%BB%BB��B�B�B�yB�sB�`B�ZB��B��B��B��B�XB�FB��B��B��B�LB��B� BdZB+B�B
��B
B
~�B
aHB
N�B
2-B
�B
B	��B	�B	��B	�?B	��B	�bB	�DB	r�B	I�B	/B	B�mB�#B��BĜB�LB��B��B��B�uB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B�B�B�B�B�B�B�B�B�B�B��B��B�B�B�3B�jB��B�dB�9B�!B�9B�3B�-B�'B�-B�'B�-B�3B�FB�dB�^B�qBȴB��B�B�B�/B�5B�;B�BB�BB�NB�HB�;B�5B�;B�ZB�ZB�ZB�`B�ZB�HB�HB�HB�BB�BB�;B�;B�;B�;B�;B�NB�ZB�sB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	DB	DB	\B	hB	uB	{B	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	!�B	#�B	$�B	'�B	1'B	2-B	49B	:^B	@�B	B�B	E�B	H�B	H�B	F�B	E�B	E�B	E�B	F�B	H�B	I�B	P�B	[#B	^5B	aHB	dZB	bNB	bNB	cTB	dZB	e`B	e`B	k�B	k�B	m�B	p�B	u�B	y�B	}�B	~�B	�B	�=B	�DB	�VB	�VB	�\B	�VB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�-B	�-B	�3B	�3B	�3B	�9B	�?B	�FB	�FB	�FB	�LB	�XB	�dB	�jB	�}B	�}B	�}B	�jB	�^B	�dB	�wB	��B	B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�;B	�BB	�NB	�TB	�TB	�TB	�TB	�`B	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
1B
�B
# 22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�)B�)B�)B�)B�)B�)B�)B�/B�/B�/B�5B�HB�mB�B��B	{B	G�B	t�B	��B	��B	��B	�/B	�B	��B
	7B
uB
�B
&�B
+B
/B
33B
6FB
:^B
I�B
ZB
jB
�B
��B
ɺB
�B
�sB
�B
�B
�B
�B+B'�B0!B5?B@�BVB\)B^5BffBv�B�hB�'B�qB��B��B�B�HB�B��B�B�ZB�B��B  BB+B
=B	7B+B%BBB%BB%BB��B�B�B�yB�sB�`B�ZB��B��B��B��B�XB�FB��B��B��B�LB��B� BdZB+B�B
��B
B
~�B
aHB
N�B
2-B
�B
B	��B	�B	��B	�?B	��B	�bB	�DB	r�B	I�B	/B	B�mB�#B��BĜB�LB��B��B��B�uB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B�B�B�B�B�B�B�B�B�B�B��B��B�B�B�3B�jB��B�dB�9B�!B�9B�3B�-B�'B�-B�'B�-B�3B�FB�dB�^B�qBȴB��B�B�B�/B�5B�;B�BB�BB�NB�HB�;B�5B�;B�ZB�ZB�ZB�`B�ZB�HB�HB�HB�BB�BB�;B�;B�;B�;B�;B�NB�ZB�sB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	DB	DB	\B	hB	uB	{B	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	!�B	#�B	$�B	'�B	1'B	2-B	49B	:^B	@�B	B�B	E�B	H�B	H�B	F�B	E�B	E�B	E�B	F�B	H�B	I�B	P�B	[#B	^5B	aHB	dZB	bNB	bNB	cTB	dZB	e`B	e`B	k�B	k�B	m�B	p�B	u�B	y�B	}�B	~�B	�B	�=B	�DB	�VB	�VB	�\B	�VB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�-B	�-B	�3B	�3B	�3B	�9B	�?B	�FB	�FB	�FB	�LB	�XB	�dB	�jB	�}B	�}B	�}B	�jB	�^B	�dB	�wB	��B	B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�;B	�BB	�NB	�TB	�TB	�TB	�TB	�`B	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
1B
�B
# 22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190550                              AO  ARCAADJP                                                                    20181005190550    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190550  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190550  QCF$                G�O�G�O�G�O�8000            