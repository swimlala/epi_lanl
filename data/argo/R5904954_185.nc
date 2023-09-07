CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:31Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191731  20181005191731  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�ݤ��Z91   @�ݥI���@6a���o�d�5?|�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A���A���A�  A�  A�  A���A�  B   B  B  B  B   B'��B/��B8  B@  BH  BP  BXffB`ffBh  Bo��Bx  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  C   C�fC�fC�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ�C\  C^  C`�Cb�Cd�Cf�Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz�C|�C~  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C��C��C��C�  C��C�  C�  C��3C��3C�  C��C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C��C��3C��3C�  C�  C�  C��3C��3C��C�  C��3C�  C��3C��C�  C��C��C�  C��C��3C��C�  C��C�  C��3C��3C��fC�  C��3C��C�  C��C�  C��C�  C��C�  C��3C��3C��3C��C�  C��C��C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��fC��fC�  C�  C��C�  C��3C�  C�  C��3C��3C�  C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3D   D �fDfD� D  D� D��D� D�D� D  D�fDfD� D��D� D  D� D��D	� D
  D
� D  D� D��Dy�D  D�fDfD�fDfD�fDfD� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D� DfD$fD$�fD%fD%� D&  D&�fD'  D'� D(fD(�fD)fD)y�D)��D*� D*��D+y�D,  D,y�D-  D-y�D.  D.� D.��D/y�D/��D0y�D0��D1� D2  D2y�D2��D3y�D4  D4y�D5  D5� D6  D6y�D6��D7� D8  D8y�D9  D9�fD:  D:� D;fD;� D<  D<� D=  D=� D>fD>�fD?fD?� D@  D@�fDA  DAy�DB  DB�fDCfDC�fDD  DDy�DD��DE� DF  DF� DG  DGy�DH  DH� DI  DI� DI��DJy�DJ��DK� DL  DLy�DL��DM� DN  DNy�DO  DO�fDPfDP� DP��DQ� DRfDR��DS  DS� DS��DTy�DT��DU� DV�DV�fDW  DWy�DW��DXy�DY  DY� DZ  DZ� D[  D[� D[��D\� D\��D]y�D]��D^y�D_  D_�fD`  D`y�D`��Da�fDbfDb�fDc  Dc� DdfDd� De  De�fDf  Df�fDg  Dg� DhfDh�fDifDi�fDjfDjy�Dj�3Dky�Dk��Dly�Dm  Dm� DnfDn�fDo  Doy�Do�3Dpy�Dq  Dqy�Dr  Dry�DsfDs� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dw� Dy��D�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A���A���A�(�A�(�A�(�A���A�(�B{B	{B{B{B!{B(�B0�B9{BA{BI{BQ{BYz�Baz�Bi{Bp�By{B��=B��=B��=B��=B�W
B��=B��pB��=B��=B��=B��=B�W
B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�W
B�W
B�W
B��=B�=B��=B��=C EC+�C+�C+�CEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECT+�CVECXECZ^�C\EC^EC`^�Cb^�Cd^�Cf^�ChECjEClECnECp+�CrECtECvECxECz^�C|^�C~EC�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�/\C�"�C�"�C�"�C�/\C�/\C�/\C�"�C�/\C�"�C�"�C��C��C�"�C�/\C�"�C�"�C�/\C�"�C��C�"�C�"�C��C�"�C�"�C�"�C�"�C�/\C��C��C�"�C�"�C�"�C��C��C�/\C�"�C��C�"�C��C�/\C�"�C�/\C�/\C�"�C�/\C��C�/\C�"�C�/\C�"�C��C��C��C�"�C��C�/\C�"�C�/\C�"�C�/\C�"�C�/\C�"�C��C��C��C�/\C�"�C�/\C�/\C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C��C�"�C�/\C�/\C�/\C�"�C�"�C�/\C�"�C��C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�/\C�"�C��C�"�C�"�C��C��C�"�C��C��C�"�C�"�C��C��C��C��C��C��D HD ��D�D�HDHD�HD
�D�HDD�HDHD��D�D�HD
�D�HDHD�HD	
�D	�HD
HD
�HDHD�HD
�D��DHD��D�D��D�D��D�D�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HD
�D�HD�D$�D$��D%�D%�HD&HD&��D'HD'�HD(�D(��D)�D)��D*
�D*�HD+
�D+��D,HD,��D-HD-��D.HD.�HD/
�D/��D0
�D0��D1
�D1�HD2HD2��D3
�D3��D4HD4��D5HD5�HD6HD6��D7
�D7�HD8HD8��D9HD9��D:HD:�HD;�D;�HD<HD<�HD=HD=�HD>�D>��D?�D?�HD@HD@��DAHDA��DBHDB��DC�DC��DDHDD��DE
�DE�HDFHDF�HDGHDG��DHHDH�HDIHDI�HDJ
�DJ��DK
�DK�HDLHDL��DM
�DM�HDNHDN��DOHDO��DP�DP�HDQ
�DQ�HDR�DR�DSHDS�HDT
�DT��DU
�DU�HDVDV��DWHDW��DX
�DX��DYHDY�HDZHDZ�HD[HD[�HD\
�D\�HD]
�D]��D^
�D^��D_HD_��D`HD`��Da
�Da��Db�Db��DcHDc�HDd�Dd�HDeHDe��DfHDf��DgHDg�HDh�Dh��Di�Di��Dj�Dj��Dk{Dk��Dl
�Dl��DmHDm�HDn�Dn��DoHDo��Dp{Dp��DqHDq��DrHDr��Ds�Ds�HDtHDt�HDuHDu�HDvHDv��DwHDw�HDw�HDy�4D�>�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A·+AΉ7A΋DA΍PA΍PAΏ\AΏ\AΏ\A΍PAΏ\AΏ\AΏ\A·+AΉ7A΋DA΃A�E�A�33A�1'A�/A�+A��A�A���A���A���A��A��A��A��A��A��yA��`A��`A��yA��A��A��A��A���A���A���A���A���A���A���A�A�1A�1A�
=A�{A�A��#A�|�A��yḀ�A�-A��
A�E�Aƥ�A���A�ZA���A�-A��A��A��A�t�A��FA���A�-A���A���A�n�A���A��DA�1A���A���A��A�~�A��;A��A���A�C�A���A�A��A�v�A��`A�K�A�7LA��`A�1'A�x�A���A�XA�  A�ȴA�ffA��HA�ƨA���A���A�A�ffA�ĜA�VA��A���A���A�t�A���A��^A�5?A��!A�-A���A�  A�A���A�Q�A�"�A�~�A���A��wA���A�oA���A���A�dZA�JA��^A~bA|ZAy+As��ArȴAq�Ap�`Ao�
An��Ai�
Ah{AgdZAf��Ae�#Ad�DAc��Ab��A_��A^ZA\1'AZ�AY�;AX9XAU/AS/ARn�AQ"�AN(�AMoAL9XAJĜAI`BAHn�AG��AG+AF��AE�AC��AC;dABjAA�A?��A=�A<�\A:��A9��A9K�A7�A6�/A5��A4~�A4-A2��A0n�A/ƨA.��A-33A,JA+hsA*��A*�+A*JA(��A'`BA&M�A$��A"��A!x�A �\A��AXAA��A�A��A�+AXA?}A��A  A�A��A�AK�A�/AZA��A��A+A�9A�;AE�A�HA=qA��A&�A
�RA
I�A	�mA	�A	`BA	&�A	�A�!A|�AA�9Az�A-A�wAG�A�yA �/A (�@�@�9X@��@�r�@���@�S�@�K�@��@��H@�u@�b@��@ꟾ@��@�
=@��@�X@�@�33@�@�I�@�{@݁@�7L@ܣ�@���@�ff@���@և+@� �@�ff@��@϶F@��y@�=q@�/@�V@�%@���@�I�@�ƨ@��y@���@�S�@ź^@őh@őh@ř�@Ł@ļj@�\)@�{@�J@��@��
@��-@�1@���@��
@�^5@�(�@�t�@���@���@��+@�V@�E�@�=q@�S�@��H@��
@�  @��@�@���@���@�?}@��`@��u@�1@��m@��
@���@�t�@�l�@�C�@��@��y@�ȴ@��R@��+@�C�@�Ĝ@��m@���@��@�z�@��u@�/@��h@��T@�{@��^@�hs@�X@�G�@�&�@��D@�Z@��@��;@��F@�o@���@�~�@�M�@�5?@�@��^@��@�%@�%@�Ĝ@�bN@�9X@�b@��@�l�@�
=@���@���@�n�@�M�@���@���@�p�@�O�@�?}@�%@��/@���@��j@�bN@��@��@��P@�S�@�o@��H@��@���@���@��\@�~�@�V@��@���@��T@�@�`B@��@�Ĝ@���@��@�9X@��
@��@�+@��@��!@���@���@��+@�ff@�-@�@��@��T@��^@�&�@��j@���@�r�@��@�ƨ@�S�@�C�@�"�@�@��H@��+@�$�@��@�@���@���@�`B@�/@���@��`@��/@�Ĝ@��D@�A�@��;@��@�dZ@��R@���@���@�~�@�^5@��T@�X@���@���@��9@���@��@�1'@�(�@�1'@���@���@���@�A�@{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A·+AΉ7A΋DA΍PA΍PAΏ\AΏ\AΏ\A΍PAΏ\AΏ\AΏ\A·+AΉ7A΋DA΃A�E�A�33A�1'A�/A�+A��A�A���A���A���A��A��A��A��A��A��yA��`A��`A��yA��A��A��A��A���A���A���A���A���A���A���A�A�1A�1A�
=A�{A�A��#A�|�A��yḀ�A�-A��
A�E�Aƥ�A���A�ZA���A�-A��A��A��A�t�A��FA���A�-A���A���A�n�A���A��DA�1A���A���A��A�~�A��;A��A���A�C�A���A�A��A�v�A��`A�K�A�7LA��`A�1'A�x�A���A�XA�  A�ȴA�ffA��HA�ƨA���A���A�A�ffA�ĜA�VA��A���A���A�t�A���A��^A�5?A��!A�-A���A�  A�A���A�Q�A�"�A�~�A���A��wA���A�oA���A���A�dZA�JA��^A~bA|ZAy+As��ArȴAq�Ap�`Ao�
An��Ai�
Ah{AgdZAf��Ae�#Ad�DAc��Ab��A_��A^ZA\1'AZ�AY�;AX9XAU/AS/ARn�AQ"�AN(�AMoAL9XAJĜAI`BAHn�AG��AG+AF��AE�AC��AC;dABjAA�A?��A=�A<�\A:��A9��A9K�A7�A6�/A5��A4~�A4-A2��A0n�A/ƨA.��A-33A,JA+hsA*��A*�+A*JA(��A'`BA&M�A$��A"��A!x�A �\A��AXAA��A�A��A�+AXA?}A��A  A�A��A�AK�A�/AZA��A��A+A�9A�;AE�A�HA=qA��A&�A
�RA
I�A	�mA	�A	`BA	&�A	�A�!A|�AA�9Az�A-A�wAG�A�yA �/A (�@�@�9X@��@�r�@���@�S�@�K�@��@��H@�u@�b@��@ꟾ@��@�
=@��@�X@�@�33@�@�I�@�{@݁@�7L@ܣ�@���@�ff@���@և+@� �@�ff@��@϶F@��y@�=q@�/@�V@�%@���@�I�@�ƨ@��y@���@�S�@ź^@őh@őh@ř�@Ł@ļj@�\)@�{@�J@��@��
@��-@�1@���@��
@�^5@�(�@�t�@���@���@��+@�V@�E�@�=q@�S�@��H@��
@�  @��@�@���@���@�?}@��`@��u@�1@��m@��
@���@�t�@�l�@�C�@��@��y@�ȴ@��R@��+@�C�@�Ĝ@��m@���@��@�z�@��u@�/@��h@��T@�{@��^@�hs@�X@�G�@�&�@��D@�Z@��@��;@��F@�o@���@�~�@�M�@�5?@�@��^@��@�%@�%@�Ĝ@�bN@�9X@�b@��@�l�@�
=@���@���@�n�@�M�@���@���@�p�@�O�@�?}@�%@��/@���@��j@�bN@��@��@��P@�S�@�o@��H@��@���@���@��\@�~�@�V@��@���@��T@�@�`B@��@�Ĝ@���@��@�9X@��
@��@�+@��@��!@���@���@��+@�ff@�-@�@��@��T@��^@�&�@��j@���@�r�@��@�ƨ@�S�@�C�@�"�@�@��H@��+@�$�@��@�@���@���@�`B@�/@���@��`@��/@�Ĝ@��D@�A�@��;@��@�dZ@��R@���@���@�~�@�^5@��T@�X@���@���@��9@���@��@�1'@�(�@�1'@���@���@���@�A�@{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��BB+B+B+B+B%B%BBBBBBBBBB%B%B	7B	7B
=B
=B
=BDBDBDBDBDBDBJBPBVBVB\B{B�B%�B,B.B-B+B9XBXB_;Bn�Bo�Bt�B|�B��B�FB�wB�qB�wB��B�wB�wBĜB��B��B��B��B��BƨBÖBBŢB��B�dB�RB�-B��B�{B�1B�Bz�Bn�BVB?}B�BDB+B��B�B�BƨB�?B��B��B��B�VB�B|�Bx�Bt�Bs�Bm�B_;BI�B7LB'�B�B�BJB
�B
�sB
�ZB
�BB
�B
��B
��B
ȴB
�}B
�!B
��B
��B
��B
��B
� B
p�B
VB
33B
)�B
#�B
�B
uB
1B	�B	�;B	�B	��B	��B	ĜB	�qB	�?B	��B	��B	�VB	�+B	�B	w�B	iyB	aHB	^5B	W
B	I�B	C�B	>wB	7LB	1'B	-B	)�B	%�B	 �B	�B	\B	DB	+B��B��B�B�mB�`B�BB�;B�)B�)B�B��B��B��BŢBB�}B�RB�3B�'B�B�B�B��B��B��B��B��B��B�oB�hB�VB�JB�DB�7B�+B�B�Bz�Bv�Bs�Bq�Bo�Bm�Bk�BjBiyBgmBffBdZBbNB_;B\)BYBW
BVBT�BR�BR�BS�BXB[#B^5BffBe`BcTBcTBdZBdZBffBgmBiyBdZB\)BYBVBT�BN�BM�BS�BYB\)BZBT�BP�BO�BN�BI�BE�BA�B?}B>wB?}B@�BB�BE�BF�BJ�BK�BL�BN�BQ�BR�BL�BE�BC�BG�BK�BN�BO�BQ�BR�BR�BR�BR�BR�BXBZB^5B]/BbNBffBffBffBdZBbNBaHBhsBl�Bl�Bl�B�RB�B�B��B��B��B�B�?B�RB�wBB��B�TB�B��B	  B	1B	
=B	VB	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	%�B	'�B	(�B	+B	.B	33B	<jB	G�B	F�B	D�B	B�B	F�B	J�B	R�B	YB	]/B	bNB	dZB	e`B	e`B	e`B	e`B	k�B	m�B	q�B	q�B	u�B	y�B	~�B	�B	�+B	�1B	�1B	�=B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�3B	�FB	�RB	�XB	�qB	�}B	B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�/B	�/B	�/B	�/B	�;B	�HB	�NB	�ZB	�fB	�fB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��BB+B+B+B+B%B%BBBBBBBBBB%B%B	7B	7B
=B
=B
=BDBDBDBDBDBDBJBPBVBVB\B{B�B%�B,B.B-B+B9XBXB_;Bn�Bo�Bt�B|�B��B�FB�wB�qB�wB��B�wB�wBĜB��B��B��B��B��BƨBÖBBŢB��B�dB�RB�-B��B�{B�1B�Bz�Bn�BVB?}B�BDB+B��B�B�BƨB�?B��B��B��B�VB�B|�Bx�Bt�Bs�Bm�B_;BI�B7LB'�B�B�BJB
�B
�sB
�ZB
�BB
�B
��B
��B
ȴB
�}B
�!B
��B
��B
��B
��B
� B
p�B
VB
33B
)�B
#�B
�B
uB
1B	�B	�;B	�B	��B	��B	ĜB	�qB	�?B	��B	��B	�VB	�+B	�B	w�B	iyB	aHB	^5B	W
B	I�B	C�B	>wB	7LB	1'B	-B	)�B	%�B	 �B	�B	\B	DB	+B��B��B�B�mB�`B�BB�;B�)B�)B�B��B��B��BŢBB�}B�RB�3B�'B�B�B�B��B��B��B��B��B��B�oB�hB�VB�JB�DB�7B�+B�B�Bz�Bv�Bs�Bq�Bo�Bm�Bk�BjBiyBgmBffBdZBbNB_;B\)BYBW
BVBT�BR�BR�BS�BXB[#B^5BffBe`BcTBcTBdZBdZBffBgmBiyBdZB\)BYBVBT�BN�BM�BS�BYB\)BZBT�BP�BO�BN�BI�BE�BA�B?}B>wB?}B@�BB�BE�BF�BJ�BK�BL�BN�BQ�BR�BL�BE�BC�BG�BK�BN�BO�BQ�BR�BR�BR�BR�BR�BXBZB^5B]/BbNBffBffBffBdZBbNBaHBhsBl�Bl�Bl�B�RB�B�B��B��B��B�B�?B�RB�wBB��B�TB�B��B	  B	1B	
=B	VB	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	%�B	'�B	(�B	+B	.B	33B	<jB	G�B	F�B	D�B	B�B	F�B	J�B	R�B	YB	]/B	bNB	dZB	e`B	e`B	e`B	e`B	k�B	m�B	q�B	q�B	u�B	y�B	~�B	�B	�+B	�1B	�1B	�=B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�3B	�FB	�RB	�XB	�qB	�}B	B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�/B	�/B	�/B	�/B	�;B	�HB	�NB	�ZB	�fB	�fB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191731                              AO  ARCAADJP                                                                    20181005191731    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191731  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191731  QCF$                G�O�G�O�G�O�8000            