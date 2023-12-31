CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:05Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190605  20181005190605  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              
A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��饼��1   @���8� @2&fffff�c�~��"�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     
A   A   A   @�33@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B ffB  B��B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C��C�  C�  C��C��3C��3C�  C��C��C��C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� DfD� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D	y�D	��D
y�D
��D� D  D� D  D� D  D�fDfD� D  D�fD  D�fD  D� D  D� D  D� D  D� D  Dy�D��D� DfD� D��D� D  D� D  D� D  Dy�D  D� D��D� D  D� D��D y�D!fD!�fD"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D6��D7� D8  D8y�D9  D9�fD:  D:y�D;  D;� D<  D<� D<��D=� D>  D>� D?fD?�fD@fD@� DA  DAy�DB  DB� DCfDC� DD  DD�fDEfDE�fDFfDF� DF��DGy�DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR�fDSfDS� DT  DT� DU  DUy�DU��DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\�fD]  D]�fD^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dcy�Dc��Ddy�De  De� De��Df� Df��Dgy�Dg��Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Dn��Doy�Do��Dpy�Dq  Dq�fDr  Dr� DsfDs� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dy�)D�,{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@ȣ�A�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A���A�(�Bz�B	{B�B{B!{B){B1{B8�BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B�pB�=B�=B�W
B�W
B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2^�C4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECp^�Cr^�CtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C��C�"�C�/\C��C��C��C��C��C��C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�/\C��C��C�"�C�/\C�/\C�/\C�"�C�"�C�/\C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HD
�D�HD�D�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HD	
�D	��D

�D
��D
�D�HDHD�HDHD�HDHD��D�D�HDHD��DHD��DHD�HDHD�HDHD�HDHD�HDHD��D
�D�HD�D�HD
�D�HDHD�HDHD�HDHD��DHD�HD
�D�HDHD�HD 
�D ��D!�D!��D"HD"�HD#�D#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1��D2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6��D7
�D7�HD8HD8��D9HD9��D:HD:��D;HD;�HD<HD<�HD=
�D=�HD>HD>�HD?�D?��D@�D@�HDAHDA��DBHDB�HDC�DC�HDDHDD��DE�DE��DF�DF�HDG
�DG��DHHDH�HDIHDI�HDJ
�DJ�HDKHDK�HDLHDL�HDM�DM�HDNHDN�HDOHDO�HDPHDP��DQHDQ�HDRHDR��DS�DS�HDTHDT�HDUHDU��DV
�DV�HDWHDW��DXHDX�HDYHDY�HDZHDZ�HD[�D[�HD\HD\��D]HD]��D^HD^�HD_HD_�HD`HD`�HDaHDa��DbHDb�HDcHDc��Dd
�Dd��DeHDe�HDf
�Df�HDg
�Dg��Dh
�Dh�HDiHDi�HDjHDj��DkHDk�HDlHDl�HDmHDm��DnHDn�HDo
�Do��Dp
�Dp��DqHDq��DrHDr�HDs�Ds�HDt
�Dt�HDuHDu�HDvHDv�HDwHDw�HDw�Dy�qD�511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�&�A�33A�+A�-A�/A�-A�-A�/A�/A�/A�-A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�I�A�I�A�I�A�K�A�K�A�M�A�O�A�O�A�Q�A�Q�A�M�A�Q�A�VA�O�A�K�A�M�A�K�A�E�A�E�A�7LA�-A�+A�A�n�A� �A�%Aǲ-A�dZA�z�Aŕ�A�r�A�l�A�hsA�G�A���A�&�A���A���A�+A�n�A��yA��/A�9XA��A��RA�t�A��7A�ffA��A���A���A��A���A�`BA��`A��A��A��A��A��A��7A�`BA��hA�ffA�XA��\A���A�G�A�ffA�A�A��A�1A��jA� �A��A�n�A���A��
A���A���A�bA~~�A{l�Ay�Av$�Ap��Am��AljAg�Af��AfffAc+Ab(�AaC�A`�A_G�A]A\�!A[t�AYx�ARZAPI�ALbNAGK�AF-AD�ACK�A@VA>A�A<��A8��A5�A4r�A3��A3l�A3"�A2�/A1��A0�/A/t�A-��A+��A)��A'%A&z�A$ �A"9XA!��A!S�A �A��A�uA�;Ap�A�!A�9A�!AVAXA~�A�A��A5?A�A`BA%AȴA�uAA�A�A�A��AffA�#A��A�A��AdZAZAz�AM�A��A��A^5AffA5?A�A��A�AS�Ap�A�FA
��A
�uA	K�A��AZA��A��A�AA-A+AĜAoA�PA Q�A �\A;dA�7A ��@�^5@���@��H@���@�C�@�~�@�@��h@�v�@��@�"�@�1@�J@�v�@�%@�F@�1@�D@�\)@��@�@���@�@�@�$�@�{@��@�5?@���@�l�@��@��/@��T@�Q�@�9X@��@柾@�O�@�I�@��@�@�o@�+@��@�7L@��@�?}@���@�  @߾w@�@�V@��@�&�@ܣ�@��@۶F@ە�@�C�@�o@���@ڏ\@�-@�p�@�b@׮@�l�@�o@���@�^5@�5?@��@թ�@Չ7@��@�Ĝ@Լj@�1'@�ƨ@�dZ@҇+@с@�j@��;@Ϯ@ύP@ύP@�+@��@���@�t�@υ@�
=@��@���@���@��H@���@�~�@�{@��#@�X@���@̃@��@�@�7L@ȓu@�1'@�r�@��`@�V@��@�%@Ȭ@�Z@�t�@�$�@�O�@�7L@�&�@�&�@��/@�Z@�I�@�bN@ă@�I�@��;@���@þw@�"�@�{@��7@�p�@�/@�Ĝ@��@�C�@���@�5?@��h@�X@�G�@���@�Ĝ@�bN@��@�;d@��!@�$�@���@��j@�9X@�
=@�-@��-@�7L@���@� �@�b@���@��m@��;@��@�@��R@���@���@�v�@�M�@�-@��T@���@��@�b@�S�@���@��+@�@��7@��@�r�@��@��F@�dZ@��R@�V@�-@�5?@���@��h@�7L@���@��`@�O�@��`@���@�j@�(�@�1@���@��@�K�@��y@���@��+@�$�@��@��@��j@��u@�(�@��@�S�@�33@�+@�~�@�-@���@��T@��-@�?}@���@�Ĝ@���@�j@�1'@� �@���@�dZ@�\)@�33@��y@�^5@�O�@���@��9@��@�Z@��@���@�K�@��@��\@�V@�E�@�5?@�@�X@��@��@���@�b@��F@��@�t�@�S�@��@���@�n�@�5?@�J@��@��-@���@�  @��w@��@�\)@�K�@�K�@��y@�^5@�@���@�x�@�%@��/@�Ĝ@�r�@�1@���@��
@��+@��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�&�A�33A�+A�-A�/A�-A�-A�/A�/A�/A�-A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�I�A�I�A�I�A�K�A�K�A�M�A�O�A�O�A�Q�A�Q�A�M�A�Q�A�VA�O�A�K�A�M�A�K�A�E�A�E�A�7LA�-A�+A�A�n�A� �A�%Aǲ-A�dZA�z�Aŕ�A�r�A�l�A�hsA�G�A���A�&�A���A���A�+A�n�A��yA��/A�9XA��A��RA�t�A��7A�ffA��A���A���A��A���A�`BA��`A��A��A��A��A��A��7A�`BA��hA�ffA�XA��\A���A�G�A�ffA�A�A��A�1A��jA� �A��A�n�A���A��
A���A���A�bA~~�A{l�Ay�Av$�Ap��Am��AljAg�Af��AfffAc+Ab(�AaC�A`�A_G�A]A\�!A[t�AYx�ARZAPI�ALbNAGK�AF-AD�ACK�A@VA>A�A<��A8��A5�A4r�A3��A3l�A3"�A2�/A1��A0�/A/t�A-��A+��A)��A'%A&z�A$ �A"9XA!��A!S�A �A��A�uA�;Ap�A�!A�9A�!AVAXA~�A�A��A5?A�A`BA%AȴA�uAA�A�A�A��AffA�#A��A�A��AdZAZAz�AM�A��A��A^5AffA5?A�A��A�AS�Ap�A�FA
��A
�uA	K�A��AZA��A��A�AA-A+AĜAoA�PA Q�A �\A;dA�7A ��@�^5@���@��H@���@�C�@�~�@�@��h@�v�@��@�"�@�1@�J@�v�@�%@�F@�1@�D@�\)@��@�@���@�@�@�$�@�{@��@�5?@���@�l�@��@��/@��T@�Q�@�9X@��@柾@�O�@�I�@��@�@�o@�+@��@�7L@��@�?}@���@�  @߾w@�@�V@��@�&�@ܣ�@��@۶F@ە�@�C�@�o@���@ڏ\@�-@�p�@�b@׮@�l�@�o@���@�^5@�5?@��@թ�@Չ7@��@�Ĝ@Լj@�1'@�ƨ@�dZ@҇+@с@�j@��;@Ϯ@ύP@ύP@�+@��@���@�t�@υ@�
=@��@���@���@��H@���@�~�@�{@��#@�X@���@̃@��@�@�7L@ȓu@�1'@�r�@��`@�V@��@�%@Ȭ@�Z@�t�@�$�@�O�@�7L@�&�@�&�@��/@�Z@�I�@�bN@ă@�I�@��;@���@þw@�"�@�{@��7@�p�@�/@�Ĝ@��@�C�@���@�5?@��h@�X@�G�@���@�Ĝ@�bN@��@�;d@��!@�$�@���@��j@�9X@�
=@�-@��-@�7L@���@� �@�b@���@��m@��;@��@�@��R@���@���@�v�@�M�@�-@��T@���@��@�b@�S�@���@��+@�@��7@��@�r�@��@��F@�dZ@��R@�V@�-@�5?@���@��h@�7L@���@��`@�O�@��`@���@�j@�(�@�1@���@��@�K�@��y@���@��+@�$�@��@��@��j@��u@�(�@��@�S�@�33@�+@�~�@�-@���@��T@��-@�?}@���@�Ĝ@���@�j@�1'@� �@���@�dZ@�\)@�33@��y@�^5@�O�@���@��9@��@�Z@��@���@�K�@��@��\@�V@�E�@�5?@�@�X@��@��@���@�b@��F@��@�t�@�S�@��@���@�n�@�5?@�J@��@��-@���@�  @��w@��@�\)@�K�@�K�@��y@�^5@�@���@�x�@�%@��/@�Ĝ@�r�@�1@���@��
@��+@��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�DB�=B�=B�=B�=B�DB�DB�=B�DB�DB�DB�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�DB�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�DB�DB�=B�1B�B�%BÖB	  B	PB	7LB	}�B	��B	�B	�jB	�/B	��B
B
XB
|�B5?Bw�B��B�B�3B�3B�3B�9B�^B�^B��B��By�B_;BdZB�+B�hB�B5?BdZBu�Bz�BdZB;dB.B+B&�B�B
�B
ɺB
�dB
��B
��B
x�B
J�B
49B
 �B	��B	��B	ÖB	�9B	��B	�VB	�B	w�B	l�B	bNB	aHB	ZB	R�B	A�B	<jB	9XB	,B	%�B	 �B	�B	�B	PB	+B��B�B�#B�TB�mB�B��B��B�B��BB�qB�XB�LB�?B�?B�?B�?B�?B�qB�wB�}B��B��B�}B�jB�RB�B�B�B�3B�^B��B��B��B�B�5B�BB�HB�TB�;B�#B�B�#B�#B�#B�HB�NB�TB�NB�ZB�mB��B��B��B��B	B	%B	VB	�B	bB	%B	PB		7B	B	%B	PB	bB	VB	PB	�B	�B	"�B	33B	5?B	:^B	6FB	1'B	0!B	,B	)�B	<jB	B�B	>wB	8RB	33B	)�B	#�B	!�B	+B	6FB	;dB	>wB	=qB	=qB	5?B	-B	(�B	&�B	%�B	&�B	/B	@�B	-B	#�B	2-B	>wB	;dB	7LB	/B	$�B	!�B	 �B	"�B	"�B	+B	2-B	33B	49B	5?B	7LB	?}B	K�B	S�B	cTB	t�B	�7B	�=B	�7B	�+B	�%B	�7B	�7B	�7B	�7B	�7B	�7B	�DB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�9B	�?B	�FB	�LB	�RB	�RB	�RB	�^B	�^B	�^B	�jB	�wB	�wB	�}B	�}B	�}B	��B	ÖB	ĜB	ŢB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�BB	�HB	�HB	�BB	�;B	�5B	�)B	�)B	�/B	�/B	�;B	�BB	�HB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�sB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B
DB

=B
1B
%B
+B
+B
+B
%B
%B
B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
DB
	7B
1B
	7B
1B
1B
	7B
	7B
DB

=B
	7B
	7B
	7B

=B

=B

=B
DB
JB
DB
\B
�B
E22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�DB�=B�=B�=B�=B�DB�DB�=B�DB�DB�DB�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�DB�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�DB�DB�=B�1B�B�%BÖB	  B	PB	7LB	}�B	��B	�B	�jB	�/B	��B
B
XB
|�B5?Bw�B��B�B�3B�3B�3B�9B�^B�^B��B��By�B_;BdZB�+B�hB�B5?BdZBu�Bz�BdZB;dB.B+B&�B�B
�B
ɺB
�dB
��B
��B
x�B
J�B
49B
 �B	��B	��B	ÖB	�9B	��B	�VB	�B	w�B	l�B	bNB	aHB	ZB	R�B	A�B	<jB	9XB	,B	%�B	 �B	�B	�B	PB	+B��B�B�#B�TB�mB�B��B��B�B��BB�qB�XB�LB�?B�?B�?B�?B�?B�qB�wB�}B��B��B�}B�jB�RB�B�B�B�3B�^B��B��B��B�B�5B�BB�HB�TB�;B�#B�B�#B�#B�#B�HB�NB�TB�NB�ZB�mB��B��B��B��B	B	%B	VB	�B	bB	%B	PB		7B	B	%B	PB	bB	VB	PB	�B	�B	"�B	33B	5?B	:^B	6FB	1'B	0!B	,B	)�B	<jB	B�B	>wB	8RB	33B	)�B	#�B	!�B	+B	6FB	;dB	>wB	=qB	=qB	5?B	-B	(�B	&�B	%�B	&�B	/B	@�B	-B	#�B	2-B	>wB	;dB	7LB	/B	$�B	!�B	 �B	"�B	"�B	+B	2-B	33B	49B	5?B	7LB	?}B	K�B	S�B	cTB	t�B	�7B	�=B	�7B	�+B	�%B	�7B	�7B	�7B	�7B	�7B	�7B	�DB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�9B	�?B	�FB	�LB	�RB	�RB	�RB	�^B	�^B	�^B	�jB	�wB	�wB	�}B	�}B	�}B	��B	ÖB	ĜB	ŢB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�BB	�HB	�HB	�BB	�;B	�5B	�)B	�)B	�/B	�/B	�;B	�BB	�HB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�sB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B
DB

=B
1B
%B
+B
+B
+B
%B
%B
B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
DB
	7B
1B
	7B
1B
1B
	7B
	7B
DB

=B
	7B
	7B
	7B

=B

=B

=B
DB
JB
DB
\B
�B
E22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190605                              AO  ARCAADJP                                                                    20181005190605    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190605  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190605  QCF$                G�O�G�O�G�O�8000            