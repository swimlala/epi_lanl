CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:56Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190556  20181005190556  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����`1   @���s��@0�ȴ9X�c�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A>ffA`  A�  A�  A�  A�33A�  A�33A�  A�33B   B  B  B  B   B(ffB0ffB8  B?��BH  BP  BXffB`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C��3C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	� D
  D
� D  D� D  D� D  D� DfD� D��D� DfD� D  Dy�D  D�fD  D� D  D� D  D� D��Dy�D  D� D  D� D  D�fDfD� D  Dy�D��Dy�D  D� D  D� D  D� D��D y�D ��D!y�D!��D"� D#  D#�fD$fD$�fD%fD%�fD&  D&y�D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D.��D/� D0fD0� D1  D1� D2  D2� D2��D3y�D3��D4� D5  D5� D6fD6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?fD?� D@  D@� DA  DA� DA��DBy�DC  DC� DC��DDy�DD��DE� DF  DF� DF��DG� DH  DH� DIfDI�fDJ  DJy�DK  DK�fDL  DL� DM  DM� DNfDN� DO  DO�fDPfDP�fDQfDQ� DRfDR� DR��DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ�fD[  D[� D\  D\� D\��D]y�D^  D^� D_  D_� D_��D`y�Da  Da�fDb  Db� DcfDc�fDd  Dd� DefDe� Df  Dfy�Df��Dg� Dh  Dh�fDi  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dw�fDys3D�>�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@ȣ�AQ�A$Q�AB�RAdQ�A�(�A�(�A�(�A�\)A�(�A�\)A�(�A�\)B{B	{B{B{B!{B)z�B1z�B9{B@�BI{BQ{BYz�Ba{Bi{Bq{By{B��=B��pB��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��B�W
B��=B��=B��=BĊ=BȊ=B�W
BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��pC ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�/\C�"�C��C��C��C��C��C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�/\C�/\C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�/\C��D 
�D �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	
�D	�HD
HD
�HDHD�HDHD�HDHD�HD�D�HD
�D�HD�D�HDHD��DHD��DHD�HDHD�HDHD�HD
�D��DHD�HDHD�HDHD��D�D�HDHD��D
�D��DHD�HDHD�HDHD�HD 
�D ��D!
�D!��D"
�D"�HD#HD#��D$�D$��D%�D%��D&HD&��D'HD'�HD(�D(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.��D/
�D/�HD0�D0�HD1HD1�HD2HD2�HD3
�D3��D4
�D4�HD5HD5�HD6�D6�HD7HD7�HD8HD8��D9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>
�D>�HD?�D?�HD@HD@�HDAHDA�HDB
�DB��DCHDC�HDD
�DD��DE
�DE�HDFHDF�HDG
�DG�HDHHDH�HDI�DI��DJHDJ��DKHDK��DLHDL�HDMHDM�HDN�DN�HDOHDO��DP�DP��DQ�DQ�HDR�DR�HDS
�DS�HDTHDT�HDU�DU�HDVHDV�HDWHDW�HDXHDX�HDY�DY�HDZHDZ��D[HD[�HD\HD\�HD]
�D]��D^HD^�HD_HD_�HD`
�D`��DaHDa��DbHDb�HDc�Dc��DdHDd�HDe�De�HDfHDf��Dg
�Dg�HDhHDh��DiHDi�HDj�Dj�HDkHDk�HDlHDl�HDmHDm�HDn�Dn�HDo
�Do�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDv�Dv�HDwHDw�HDw׮Dy�{D�G\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��mA��`A��HA��yA��A��A��A���A���A���A��
A���A�A˙�AʑhA�=qA���A�;dA�{A���A�33A�A�A���A�JA�  A��A��/A��/A�oA�~�A�XAȮA��;A�Q�A�r�AƼjA��#A��/A�A�A�AǮA�;dAƙ�A�|�A�A�K�A�dZA�n�A��wA��HA��A�1A��A��A�%A��TA��A��PA��A��PA���A�ZA���A�$�A��FA�+A�t�A���A��-A�$�A�=qA��
A�x�A���A�\)A���A�`BA�JA��hA���A�E�A���A�n�A�  A��A�(�A��mA�{A�|�A�?}A�x�A���A��/A�n�A���A���A���A��mA���A�C�A�r�A{t�Au%Ar5?Ap�!Am��Ag�FAc��A^�A\bAV�AR��AQt�AN�RALVAJ�+AI"�AFVACXA@ȴA?�A?�A<z�A9O�A7�FA7�hA6��A4~�A3C�A2�RA2bA0��A/�A.5?A-|�A,n�A+�FA+|�A*A�A)G�A(~�A'K�A&�A%x�A%�A$�A$^5A#
=A"A�A!��A �9A   A�A��A|�Az�Ax�A�yAZA|�A+A�TA�A�#A��A��Av�A$�A"�A��AM�A��AO�AƨA&�AbAG�A
��A
�\A
�A
z�A	�^A��A��AI�A(�A�A?}A n�@��y@�Z@�l�@�l�@�|�@��!@��@���@�b@���@��@�33@�dZ@�E�@�%@�Z@�b@�;d@�\@�V@�7@�hs@�7@�?}@�Z@�dZ@�@��@�Z@�
=@��T@�?}@���@�Z@��H@ݑh@��@���@� �@�\)@�\)@���@ו�@�S�@�33@�^5@�&�@��/@Դ9@�Z@���@�o@�ȴ@�V@�O�@�Z@ϥ�@Η�@͉7@�9X@�V@�@�p�@���@ȃ@�  @Ǿw@�K�@���@Ƨ�@�M�@���@ŉ7@ēu@�K�@°!@�v�@�5?@��T@��@�X@�G�@�?}@�?}@�V@��9@�9X@��@��R@�ff@�-@��-@�p�@���@���@�Ĝ@���@��F@��@�K�@��!@�{@��-@�hs@��@���@���@���@���@��@���@�\)@��@�
=@��+@�^5@�E�@�=q@�@�hs@��@�Z@��@���@��;@���@��+@�M�@�ȴ@��@�;d@�K�@�o@�n�@�$�@�X@���@��`@��@�Z@���@���@��h@�V@���@��u@�Q�@��m@���@�
=@���@�J@��@��T@�?}@��j@�1@��
@���@�"�@�~�@��T@��^@��@�`B@�{@�;d@�K�@�o@�M�@��^@�?}@��@��@�&�@�?}@��@��@���@��9@���@�@���@�^5@��@�x�@��7@�/@���@�Q�@��@���@���@��@�\)@��R@�{@���@���@�@�X@��@��D@�Q�@�(�@� �@��@��H@�^5@�-@�-@���@��/@���@��u@�z�@�r�@�(�@�S�@�\)@�S�@�
=@���@���@�{@��@�hs@�O�@�%@��D@�Q�@� �@��
@�t�@�;d@�
=@��@�~�@�n�@�ff@�V@�$�@�{@��#@���@�V@�Ĝ@�Z@��P@�
=@���@�^5@�M�@�J@��#@��-@��7@��7@�hs@�7L@��@��`@���@�r�@�  @��;@�l�@���@�v�@��@�@��-@���@��h@��h@�/@�Q�@�9X@�9X@��@�1@���@���@��@�+@�@��@�@��H@��\@�n�@�=q@�J@���@��-@�x�@�X@�/@��@�V@���@�Ĝ@�bN@��@���@��w@���@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��mA��`A��HA��yA��A��A��A���A���A���A��
A���A�A˙�AʑhA�=qA���A�;dA�{A���A�33A�A�A���A�JA�  A��A��/A��/A�oA�~�A�XAȮA��;A�Q�A�r�AƼjA��#A��/A�A�A�AǮA�;dAƙ�A�|�A�A�K�A�dZA�n�A��wA��HA��A�1A��A��A�%A��TA��A��PA��A��PA���A�ZA���A�$�A��FA�+A�t�A���A��-A�$�A�=qA��
A�x�A���A�\)A���A�`BA�JA��hA���A�E�A���A�n�A�  A��A�(�A��mA�{A�|�A�?}A�x�A���A��/A�n�A���A���A���A��mA���A�C�A�r�A{t�Au%Ar5?Ap�!Am��Ag�FAc��A^�A\bAV�AR��AQt�AN�RALVAJ�+AI"�AFVACXA@ȴA?�A?�A<z�A9O�A7�FA7�hA6��A4~�A3C�A2�RA2bA0��A/�A.5?A-|�A,n�A+�FA+|�A*A�A)G�A(~�A'K�A&�A%x�A%�A$�A$^5A#
=A"A�A!��A �9A   A�A��A|�Az�Ax�A�yAZA|�A+A�TA�A�#A��A��Av�A$�A"�A��AM�A��AO�AƨA&�AbAG�A
��A
�\A
�A
z�A	�^A��A��AI�A(�A�A?}A n�@��y@�Z@�l�@�l�@�|�@��!@��@���@�b@���@��@�33@�dZ@�E�@�%@�Z@�b@�;d@�\@�V@�7@�hs@�7@�?}@�Z@�dZ@�@��@�Z@�
=@��T@�?}@���@�Z@��H@ݑh@��@���@� �@�\)@�\)@���@ו�@�S�@�33@�^5@�&�@��/@Դ9@�Z@���@�o@�ȴ@�V@�O�@�Z@ϥ�@Η�@͉7@�9X@�V@�@�p�@���@ȃ@�  @Ǿw@�K�@���@Ƨ�@�M�@���@ŉ7@ēu@�K�@°!@�v�@�5?@��T@��@�X@�G�@�?}@�?}@�V@��9@�9X@��@��R@�ff@�-@��-@�p�@���@���@�Ĝ@���@��F@��@�K�@��!@�{@��-@�hs@��@���@���@���@���@��@���@�\)@��@�
=@��+@�^5@�E�@�=q@�@�hs@��@�Z@��@���@��;@���@��+@�M�@�ȴ@��@�;d@�K�@�o@�n�@�$�@�X@���@��`@��@�Z@���@���@��h@�V@���@��u@�Q�@��m@���@�
=@���@�J@��@��T@�?}@��j@�1@��
@���@�"�@�~�@��T@��^@��@�`B@�{@�;d@�K�@�o@�M�@��^@�?}@��@��@�&�@�?}@��@��@���@��9@���@�@���@�^5@��@�x�@��7@�/@���@�Q�@��@���@���@��@�\)@��R@�{@���@���@�@�X@��@��D@�Q�@�(�@� �@��@��H@�^5@�-@�-@���@��/@���@��u@�z�@�r�@�(�@�S�@�\)@�S�@�
=@���@���@�{@��@�hs@�O�@�%@��D@�Q�@� �@��
@�t�@�;d@�
=@��@�~�@�n�@�ff@�V@�$�@�{@��#@���@�V@�Ĝ@�Z@��P@�
=@���@�^5@�M�@�J@��#@��-@��7@��7@�hs@�7L@��@��`@���@�r�@�  @��;@�l�@���@�v�@��@�@��-@���@��h@��h@�/@�Q�@�9X@�9X@��@�1@���@���@��@�+@�@��@�@��H@��\@�n�@�=q@�J@���@��-@�x�@�X@�/@��@�V@���@�Ĝ@�bN@��@���@��w@���@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�!B	�!B	�'B	�!B	�!B	�'B	�!B	�!B	�B	�B	�B	��B	�uB	�PB	� B	�B	�7B	�B	�VB	�uB	��B	�3B	��B	�B	�PB	�{B	�{B	��B	�B	�)B	�B
�B
(�B
�B
B	��B
�B
!�B
7LB
YB
�B
�B5?Bu�B��B��B�;B�B��B��B�B$�B'�B.B5?B:^BC�BT�BW
BW
BT�BR�BQ�BP�BP�BN�BL�BG�B>wB1'B'�B"�B�B�B
=BB��B��B��B�?B��B�B�-B�B��Bw�BbNBJ�B>wB'�B\B
��B
�/B
ȴB
�B
��B
�B
e`B
/B

=B	�B	�^B	�hB	}�B	q�B	]/B	:^B	 �B		7B�B�5B��BŢB�dB�'B�B��B��B�{B�\B�PB�uB�bB�+B�7B�+B�B�%B�1B�+B�B�B�%B�1B�=B�bB��B��B��B��B�{B�{B��B��B��B��B��B�-B�LB�RB�}BŢBƨBƨBƨBƨBĜBBĜB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�;B�fB�B�B�B�BB��BɺBȴBȴBǮBŢBǮBȴB��B��BɺBȴBŢBǮB��B��B��BǮBƨBƨBƨBǮB��B��B��B��B��B�B�
B�B�;B�BB�HB�`B�sB�B�B�B�B�B�B�B�B�B�B�B��B��B��B	B	
=B	DB	DB	PB	VB	{B	�B	�B	�B	�B	�B	"�B	#�B	%�B	,B	.B	0!B	1'B	33B	5?B	5?B	7LB	8RB	8RB	9XB	;dB	B�B	E�B	E�B	H�B	K�B	L�B	N�B	N�B	N�B	R�B	YB	ZB	\)B	^5B	aHB	e`B	hsB	jB	jB	m�B	m�B	p�B	p�B	p�B	r�B	s�B	s�B	s�B	t�B	w�B	y�B	{�B	|�B	~�B	�B	�B	�B	�+B	�1B	�=B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�LB	�dB	�wB	��B	ƨB	ǮB	ȴB	ȴB	ƨB	ŢB	ǮB	ǮB	ǮB	ȴB	ƨB	ŢB	ĜB	ŢB	ĜB	ĜB	ÖB	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	ǮB	ǮB	ǮB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	�B	�5B	�BB	�;B	�/B	�)B	�#B	�/B	�HB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�fB	�fB	�`B	�mB	�B	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B
	7B

=B

=B

=B

=B

=B

=B
DB
JB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
oB
oB
oB
hB
bB
bB
bB
\B
\B
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
 B
/�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	�!B	�!B	�'B	�!B	�!B	�'B	�!B	�!B	�B	�B	�B	��B	�uB	�PB	� B	�B	�7B	�B	�VB	�uB	��B	�3B	��B	�B	�PB	�{B	�{B	��B	�B	�)B	�B
�B
(�B
�B
B	��B
�B
!�B
7LB
YB
�B
�B5?Bu�B��B��B�;B�B��B��B�B$�B'�B.B5?B:^BC�BT�BW
BW
BT�BR�BQ�BP�BP�BN�BL�BG�B>wB1'B'�B"�B�B�B
=BB��B��B��B�?B��B�B�-B�B��Bw�BbNBJ�B>wB'�B\B
��B
�/B
ȴB
�B
��B
�B
e`B
/B

=B	�B	�^B	�hB	}�B	q�B	]/B	:^B	 �B		7B�B�5B��BŢB�dB�'B�B��B��B�{B�\B�PB�uB�bB�+B�7B�+B�B�%B�1B�+B�B�B�%B�1B�=B�bB��B��B��B��B�{B�{B��B��B��B��B��B�-B�LB�RB�}BŢBƨBƨBƨBƨBĜBBĜB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�;B�fB�B�B�B�BB��BɺBȴBȴBǮBŢBǮBȴB��B��BɺBȴBŢBǮB��B��B��BǮBƨBƨBƨBǮB��B��B��B��B��B�B�
B�B�;B�BB�HB�`B�sB�B�B�B�B�B�B�B�B�B�B�B��B��B��B	B	
=B	DB	DB	PB	VB	{B	�B	�B	�B	�B	�B	"�B	#�B	%�B	,B	.B	0!B	1'B	33B	5?B	5?B	7LB	8RB	8RB	9XB	;dB	B�B	E�B	E�B	H�B	K�B	L�B	N�B	N�B	N�B	R�B	YB	ZB	\)B	^5B	aHB	e`B	hsB	jB	jB	m�B	m�B	p�B	p�B	p�B	r�B	s�B	s�B	s�B	t�B	w�B	y�B	{�B	|�B	~�B	�B	�B	�B	�+B	�1B	�=B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�LB	�dB	�wB	��B	ƨB	ǮB	ȴB	ȴB	ƨB	ŢB	ǮB	ǮB	ǮB	ȴB	ƨB	ŢB	ĜB	ŢB	ĜB	ĜB	ÖB	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	ǮB	ǮB	ǮB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	�B	�5B	�BB	�;B	�/B	�)B	�#B	�/B	�HB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�fB	�fB	�`B	�mB	�B	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B
	7B

=B

=B

=B

=B

=B

=B
DB
JB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
oB
oB
oB
hB
bB
bB
bB
\B
\B
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
 B
/�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190556                              AO  ARCAADJP                                                                    20181005190556    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190556  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190556  QCF$                G�O�G�O�G�O�8000            