CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:58Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191658  20181005191658  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               -A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׺��	,l1   @׺�X�@5YXbM��c��S��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      -A   A   A   @333@�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B/��B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C#�fC&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cu�fCx  Cz�C|  C}�fC��C��3C��3C��3C��3C�  C��C�  C��C��C��C�  C�  C��C��C��C�  C��3C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��fC��3C��C��C��C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��3C�  C�  C��C��3C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C��C��3C��fC��3C��C��C�  C�  C��C�  C��C��3C�  C��3C�  C�  C��C�  C��3C�  C��3C��3C�  C��C�  C��3C��3C��3C��3C��3C�  C��C��C�  C�  C�  C�  C��3C��fC�  C��C��C��C��C��C��3C��3C�  C�  C��D fD � D  D� DfDy�D  Dy�DfD� D  Dy�D��D� DfD�fDfD� D	  D	� D
  D
�fDfD�fD  Dy�D��Dy�D  D� D  D� D  D� D  D� D  D�fD  Dy�D��D� D  D� D��Dy�DfD�fD  Dy�D  D� DfD�fD  D� DfD�fD  D� D��Dy�D  D�fD   D � D!fD!�fD"  D"s3D#  D#�fD$  D$� D%  D%y�D%��D&y�D&��D'� D'��D(� D)fD)� D*  D*� D+fD+y�D+��D,�fD-fD-� D-��D.y�D/  D/y�D0  D0� D1fD1�fD2  D2�fD3fD3� D3��D4� D4��D5y�D5��D6y�D7  D7�fD8  D8� D9fD9�fD:  D:y�D;  D;y�D<  D<� D=  D=y�D>  D>�fD?fD?y�D@fD@�fD@��DAy�DB  DB�fDCfDC� DC��DD� DEfDEy�DF  DF�fDGfDG�fDH  DH�fDI  DI� DJfDJ� DKfDK� DL  DL� DMfDM�fDNfDN� DO  DO��DP  DP� DQfDQy�DQ��DRy�DS  DS��DT  DT� DT��DU� DV  DV� DW  DW� DW��DX�fDY  DY� DZfDZ��D[  D[y�D\fD\y�D]  D]� D^fD^� D_  D_�fD`�D`�fD`��Da�fDb  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDh��Di� Dj  Dj� Dk  Dk�fDl  Dly�DmfDm�fDnfDn� DofDo� Dp  Dp� Dq  Dq�fDrfDr�fDs  Dsy�Dt  Dt� Du  Duy�Dv  Dvy�Dv��Dw�fDw� Dy��D�*�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Dz�@���@�p�A�RA$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!z�B){B0�B9{BAz�BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B�W
B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B�W
B��=B��=B��=BĊ=BȊ=B̊=B�W
BԊ=B؊=B�W
B��=B�=B�=B�pB��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECEC^�C EC"EC$+�C&EC(EC*EC,EC.^�C0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZ+�C\+�C^EC`ECbECdECfEChECjEClECnECp+�CrECtECv+�CxECz^�C|EC~+�C��C��C��C��C��C�"�C�/\C�"�C�/\C�/\C�/\C�"�C�"�C�/\C�/\C�/\C�"�C��C��C��C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C��C��C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�/\C�"�C��C��C�/\C�/\C�/\C�"�C�"�C��C�"�C�/\C�"�C�"�C�/\C�"�C�"�C�"�C��C�"�C�"�C�<)C��C�"�C��C��C��C�"�C�"�C��C�"�C�"�C�"�C�/\C��C��C��C�/\C�/\C�"�C�"�C�/\C�"�C�/\C��C�"�C��C�"�C�"�C�/\C�"�C��C�"�C��C��C�"�C�/\C�"�C��C��C��C��C��C�"�C�<)C�/\C�"�C�"�C�"�C�"�C��C��C�"�C�<)C�<)C�/\C�/\C�/\C��C��C�"�C�"�C�/\D �D �HDHD�HD�D��DHD��D�D�HDHD��D
�D�HD�D��D�D�HD	HD	�HD
HD
��D�D��DHD��D
�D��DHD�HDHD�HDHD�HDHD�HDHD��DHD��D
�D�HDHD�HD
�D��D�D��DHD��DHD�HD�D��DHD�HD�D��DHD�HD
�D��DHD��D HD �HD!�D!��D"HD"�{D#HD#��D$HD$�HD%HD%��D&
�D&��D'
�D'�HD(
�D(�HD)�D)�HD*HD*�HD+�D+��D,
�D,��D-�D-�HD.
�D.��D/HD/��D0HD0�HD1�D1��D2HD2��D3�D3�HD4
�D4�HD5
�D5��D6
�D6��D7HD7��D8HD8�HD9�D9��D:HD:��D;HD;��D<HD<�HD=HD=��D>HD>��D?�D?��D@�D@��DA
�DA��DBHDB��DC�DC�HDD
�DD�HDE�DE��DFHDF��DG�DG��DHHDH��DIHDI�HDJ�DJ�HDK�DK�HDLHDL�HDM�DM��DN�DN�HDOHDO�DPHDP�HDQ�DQ��DR
�DR��DSHDS�DTHDT�HDU
�DU�HDVHDV�HDWHDW�HDX
�DX��DYHDY�HDZ�DZ�D[HD[��D\�D\��D]HD]�HD^�D^�HD_HD_��D`D`��Da
�Da��DbHDb�HDcHDc�HDd�Dd�HDeHDe�HDfHDf�HDgHDg�HDhHDh��Di
�Di�HDjHDj�HDkHDk��DlHDl��Dm�Dm��Dn�Dn�HDo�Do�HDpHDp�HDqHDq��Dr�Dr��DsHDs��DtHDt�HDuHDu��DvHDv��Dw
�Dw��Dw�HDy�>D�33D�ؤ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A׸RA׶FA׸RA׼jA׾wA׾wA׼jAײ-A׶FA���A���A���A���A���A���A���A���A�ĜA׾wA���AבhA֩�Aԕ�Aӛ�A��A�jA���A�$�A�Q�AϸRAΝ�A���A�
=Aɛ�A�A���A�5?A���Aě�Aò-A�S�A�$�A�XA��A�r�A�{A��A�~�A�9XA��
A�7LA���A�z�A�?}A�7LA�/A� �A�%A�ffA�%A�Q�A��A��A�?}A��yA�VA��yA�bA��A�t�A�5?A�5?A��A���A���A���A�1A�hsA�;dA�5?A�G�A��wA�VA�7LA�bA���A�"�A��DA�;dA�r�A�A��\A�VA��HA���A�x�A�=qA��/A�oA���A��HA���A�A�A���A�9XA��wA�1A�ȴA��\A�ĜA�E�A}�PA|I�A{O�Az��Aw�TAs��Aql�AodZAm�^Al�!AkO�AjZAh�uAf{AcS�A_x�A\n�A["�AZbNAXr�AU�-ASO�AQ�
AO�AL�AI�AG�
AG�7AGS�AF��AE;dAC�hAAO�A>�A=%A<��A;�TA;G�A9oA6��A5/A4��A3�;A2�/A0�A.�A-��A,�uA,  A+�^A+�PA+A)&�A(E�A'�A'��A'�A&ĜA%�A$��A${A"��A!�A�A��A-A��A%An�A�uA�yA�mA=qAJAbA"�A�9A�/AbA|�A�RA  A
ȴA	�hA�yA{A�A�A�uA��A�
Az�AoA��A��A�9A%A�A�7A7LA �`@�dZ@��R@���@�33@�V@��
@��H@��@���@��@�p�@��j@�v�@�  @�\@��@���@畁@�Z@�@�S�@��H@�5?@���@߶F@�o@�ȴ@�J@ۮ@���@ڗ�@�=q@ى7@��;@�33@ָR@և+@�^5@ղ-@�/@ԋD@�X@� �@��@�ƨ@�K�@с@���@϶F@��y@·+@�ff@�M�@�5?@�{@���@͡�@̋D@���@�G�@��@��@�r�@Ǿw@�ȴ@�ff@��@őh@�hs@�/@���@�bN@� �@��;@Å@�S�@��@\@�@�/@��j@� �@��@�;d@��H@�ff@�%@���@���@��@�t�@���@���@�&�@��j@�Z@�1'@�b@��m@��w@���@��@�dZ@��H@���@�~�@�J@���@��h@��@�r�@���@���@�l�@�C�@�"�@�@���@��H@���@���@��@��@�33@���@�n�@�^5@�ff@�=q@���@��-@��-@��h@���@���@��@��j@�j@�9X@� �@��@��@��R@�{@��T@�G�@��@��u@�A�@�  @���@��w@��F@���@���@���@���@��P@�33@��R@�V@�5?@�-@�J@��-@�x�@�`B@�X@�7L@���@���@�Z@�  @�dZ@�K�@�\)@�dZ@�\)@�S�@�"�@���@��R@��+@�V@�-@�@���@���@�p�@�?}@���@���@�1'@�|�@��H@��@��h@�7L@���@��@�  @��F@��@��!@���@�v�@�V@�=q@���@�5?@�5?@�{@���@��@�&�@�%@���@�r�@�j@�Z@�Q�@�1'@��w@���@�|�@�+@��H@��+@�n�@�V@��@��T@�@���@��@�p�@�p�@��@�x�@�hs@���@�Z@��m@���@�dZ@�;d@��@�~�@�$�@�@���@�X@�V@���@���@���@�j@��@���@��F@�|�@�dZ@�\)@�33@���@��@���@�n�@�^5@���@���@���@��7@�O�@�?}@�?}@�?}@�7L@�/@�&�@��@���@��9@��F@���@���@p�E1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A׸RA׶FA׸RA׼jA׾wA׾wA׼jAײ-A׶FA���A���A���A���A���A���A���A���A�ĜA׾wA���AבhA֩�Aԕ�Aӛ�A��A�jA���A�$�A�Q�AϸRAΝ�A���A�
=Aɛ�A�A���A�5?A���Aě�Aò-A�S�A�$�A�XA��A�r�A�{A��A�~�A�9XA��
A�7LA���A�z�A�?}A�7LA�/A� �A�%A�ffA�%A�Q�A��A��A�?}A��yA�VA��yA�bA��A�t�A�5?A�5?A��A���A���A���A�1A�hsA�;dA�5?A�G�A��wA�VA�7LA�bA���A�"�A��DA�;dA�r�A�A��\A�VA��HA���A�x�A�=qA��/A�oA���A��HA���A�A�A���A�9XA��wA�1A�ȴA��\A�ĜA�E�A}�PA|I�A{O�Az��Aw�TAs��Aql�AodZAm�^Al�!AkO�AjZAh�uAf{AcS�A_x�A\n�A["�AZbNAXr�AU�-ASO�AQ�
AO�AL�AI�AG�
AG�7AGS�AF��AE;dAC�hAAO�A>�A=%A<��A;�TA;G�A9oA6��A5/A4��A3�;A2�/A0�A.�A-��A,�uA,  A+�^A+�PA+A)&�A(E�A'�A'��A'�A&ĜA%�A$��A${A"��A!�A�A��A-A��A%An�A�uA�yA�mA=qAJAbA"�A�9A�/AbA|�A�RA  A
ȴA	�hA�yA{A�A�A�uA��A�
Az�AoA��A��A�9A%A�A�7A7LA �`@�dZ@��R@���@�33@�V@��
@��H@��@���@��@�p�@��j@�v�@�  @�\@��@���@畁@�Z@�@�S�@��H@�5?@���@߶F@�o@�ȴ@�J@ۮ@���@ڗ�@�=q@ى7@��;@�33@ָR@և+@�^5@ղ-@�/@ԋD@�X@� �@��@�ƨ@�K�@с@���@϶F@��y@·+@�ff@�M�@�5?@�{@���@͡�@̋D@���@�G�@��@��@�r�@Ǿw@�ȴ@�ff@��@őh@�hs@�/@���@�bN@� �@��;@Å@�S�@��@\@�@�/@��j@� �@��@�;d@��H@�ff@�%@���@���@��@�t�@���@���@�&�@��j@�Z@�1'@�b@��m@��w@���@��@�dZ@��H@���@�~�@�J@���@��h@��@�r�@���@���@�l�@�C�@�"�@�@���@��H@���@���@��@��@�33@���@�n�@�^5@�ff@�=q@���@��-@��-@��h@���@���@��@��j@�j@�9X@� �@��@��@��R@�{@��T@�G�@��@��u@�A�@�  @���@��w@��F@���@���@���@���@��P@�33@��R@�V@�5?@�-@�J@��-@�x�@�`B@�X@�7L@���@���@�Z@�  @�dZ@�K�@�\)@�dZ@�\)@�S�@�"�@���@��R@��+@�V@�-@�@���@���@�p�@�?}@���@���@�1'@�|�@��H@��@��h@�7L@���@��@�  @��F@��@��!@���@�v�@�V@�=q@���@�5?@�5?@�{@���@��@�&�@�%@���@�r�@�j@�Z@�Q�@�1'@��w@���@�|�@�+@��H@��+@�n�@�V@��@��T@�@���@��@�p�@�p�@��@�x�@�hs@���@�Z@��m@���@�dZ@�;d@��@�~�@�$�@�@���@�X@�V@���@���@���@�j@��@���@��F@�|�@�dZ@�\)@�33@���@��@���@�n�@�^5@���@���@���@��7@�O�@�?}@�?}@�?}@�7L@�/@�&�@��@���@��9@��F@���@���@p�E1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B!�B&�B9XB[#Bk�Bz�B�B�=B�JB�{B��B��B�jBĜB�/BB\B�B#�B:^B@�BT�B��B��B�VB�hB�uB�bB�bB�\B�PB�=B�\B��B��B��B�B�'B�?B�3B�B��B��B��B��B�1BjBaHBZBXBT�BR�BK�BH�BB�B>wB8RB49B0!B$�B�B
=BB��B�B�B�fB�B�}B�B^5B�BPB	7BB
��B
��B
��B
�B
�HB
B
�B
��B
�\B
�1B
�B
~�B
t�B
p�B
k�B
^5B
A�B
�B
oB
JB
B	��B	�NB	�
B	��B	B	�dB	�3B	�B	��B	�\B	|�B	iyB	XB	N�B	I�B	?}B	1'B	&�B	�B	�B	1B��B�B�B�B�B�sB�HB�B��B��B��BȴBŢB�}B�LB�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�RB�!B�B�B�-B�FB��BB��B�wB�wBÖBɺB��B��B��B��B��B��B��BǮBŢBȴBÖB�jB�FB�3B�B�B�B��B�B�B�B��B��B��B��B��B�B�B�!B�!B�-B�RB�XB�^B�dB�dB�qB�wBB��B��B�B�B�B�#B�)B�)B�)B�)B�/B�5B�;B�;B�;B�;B�BB�fB�sB�sB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	%B	1B	DB	PB	\B	hB	uB	�B	�B	�B	�B	�B	"�B	&�B	)�B	,B	.B	/B	/B	0!B	1'B	2-B	2-B	33B	8RB	<jB	>wB	B�B	D�B	E�B	I�B	K�B	N�B	P�B	Q�B	R�B	S�B	T�B	T�B	T�B	W
B	ZB	\)B	ffB	k�B	n�B	p�B	p�B	p�B	q�B	t�B	v�B	v�B	w�B	|�B	~�B	~�B	~�B	�B	�B	�B	�B	�%B	�1B	�+B	�+B	�=B	�DB	�PB	�VB	�bB	�hB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�jB	�wB	�wB	��B	��B	��B	ÖB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�TB	�`B	�mB	�sB	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B	�B
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B!�B&�B9XB[#Bk�Bz�B�B�=B�JB�{B��B��B�jBĜB�/BB\B�B#�B:^B@�BT�B��B��B�VB�hB�uB�bB�bB�\B�PB�=B�\B��B��B��B�B�'B�?B�3B�B��B��B��B��B�1BjBaHBZBXBT�BR�BK�BH�BB�B>wB8RB49B0!B$�B�B
=BB��B�B�B�fB�B�}B�B^5B�BPB	7BB
��B
��B
��B
�B
�HB
B
�B
��B
�\B
�1B
�B
~�B
t�B
p�B
k�B
^5B
A�B
�B
oB
JB
B	��B	�NB	�
B	��B	B	�dB	�3B	�B	��B	�\B	|�B	iyB	XB	N�B	I�B	?}B	1'B	&�B	�B	�B	1B��B�B�B�B�B�sB�HB�B��B��B��BȴBŢB�}B�LB�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�RB�!B�B�B�-B�FB��BB��B�wB�wBÖBɺB��B��B��B��B��B��B��BǮBŢBȴBÖB�jB�FB�3B�B�B�B��B�B�B�B��B��B��B��B��B�B�B�!B�!B�-B�RB�XB�^B�dB�dB�qB�wBB��B��B�B�B�B�#B�)B�)B�)B�)B�/B�5B�;B�;B�;B�;B�BB�fB�sB�sB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	%B	1B	DB	PB	\B	hB	uB	�B	�B	�B	�B	�B	"�B	&�B	)�B	,B	.B	/B	/B	0!B	1'B	2-B	2-B	33B	8RB	<jB	>wB	B�B	D�B	E�B	I�B	K�B	N�B	P�B	Q�B	R�B	S�B	T�B	T�B	T�B	W
B	ZB	\)B	ffB	k�B	n�B	p�B	p�B	p�B	q�B	t�B	v�B	v�B	w�B	|�B	~�B	~�B	~�B	�B	�B	�B	�B	�%B	�1B	�+B	�+B	�=B	�DB	�PB	�VB	�bB	�hB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�jB	�wB	�wB	��B	��B	��B	ÖB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�TB	�`B	�mB	�sB	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B	�B
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191658                              AO  ARCAADJP                                                                    20181005191658    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191658  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191658  QCF$                G�O�G�O�G�O�8000            