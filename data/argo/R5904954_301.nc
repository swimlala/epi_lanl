CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:58Z creation      
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191758  20181005191758  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              -A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�����'1   @����~f@57���+�d����S�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     -A   A   A   @�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�33A�  A���B   B  B  B��B��B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~�C��C�  C�  C��3C��3C�  C��3C��3C�  C��C�  C��3C��3C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C��3C�  C��C�  C�  C��C��C�  C��C�  C��3C��3C��3C��3C��3C�  C��C��C�  C��3C�  C��C��C��C�  C�  C��C��C��C�  C��3C��3C�  C�  C�  C��3C��C��C�  C��3C�  C�  C��3C�  C��C�  C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D�fD  Dy�D  Dy�DfD� D��D� D	  D	y�D
fD
�fDfD� D�D� DfD� D  D� D��D� D  Dy�D��D� D��D�fD  Ds3D��D� DfD�fD  Dy�D  D�fDfDy�DfD� D  D� D  D�fDfD� DfD� DfDy�D  Dy�D   D � D!fD!y�D!��D"y�D"��D#� D$  D$y�D%fD%� D%�3D&y�D'fD'� D(  D(y�D)  D)� D*  D*� D+  D+�fD+��D,y�D,��D-� D-��D.� D/fD/�fD0fD0� D0��D1�fD2fD2�fD3  D3�fD4  D4y�D4��D5� D6  D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;s3D<  D<��D=fD=y�D>fD>�fD?  D?y�D?��D@� DA  DAy�DB  DB� DCfDC� DD  DD� DE  DE� DF  DF�fDGfDG�fDHfDH� DI  DI�fDI��DJy�DK  DK�fDLfDL� DL��DM� DM��DN� DN��DOy�DP  DP�fDP��DQ�fDQ��DRy�DS  DSy�DS��DT� DT��DUy�DU��DV�fDWfDWy�DW��DX�fDY  DY�fDZ  DZy�D[fD[� D\fD\y�D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dgy�DhfDh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dv��DyP D�)HD��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A���A�(�A�(�A�\)A�(�A���B{B	{B{B�B �B){B1{B9z�BAz�BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B�W
B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B�W
B܊=B��=B�=B�=B�=B��=B�=B��=B��pC ECECECECEC
ECECECECECECEC^�CECEC^�C EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECH^�CJECL+�CNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECr+�CtECvECxECzEC|EC~^�C�/\C�"�C�"�C��C��C�"�C��C��C�"�C�/\C�"�C��C��C�/\C�/\C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�/\C�"�C��C�"�C�"�C��C�"�C�/\C�"�C�"�C�/\C�/\C�"�C�/\C�"�C��C��C��C��C��C�"�C�/\C�/\C�"�C��C�"�C�/\C�/\C�/\C�"�C�"�C�/\C�/\C�/\C�"�C��C��C�"�C�"�C�"�C��C�/\C�/\C�"�C��C�"�C�"�C��C�"�C�/\C�"�C��C��C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�/\C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD��DHD�HDHD�HDHD��DHD��DHD��D�D�HD
�D�HD	HD	��D
�D
��D�D�HDD�HD�D�HDHD�HD
�D�HDHD��D
�D�HD
�D��DHD�{D
�D�HD�D��DHD��DHD��D�D��D�D�HDHD�HDHD��D�D�HD�D�HD�D��DHD��D HD �HD!�D!��D"
�D"��D#
�D#�HD$HD$��D%�D%�HD&{D&��D'�D'�HD(HD(��D)HD)�HD*HD*�HD+HD+��D,
�D,��D-
�D-�HD.
�D.�HD/�D/��D0�D0�HD1
�D1��D2�D2��D3HD3��D4HD4��D5
�D5�HD6HD6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;�{D<HD<�D=�D=��D>�D>��D?HD?��D@
�D@�HDAHDA��DBHDB�HDC�DC�HDDHDD�HDEHDE�HDFHDF��DG�DG��DH�DH�HDIHDI��DJ
�DJ��DKHDK��DL�DL�HDM
�DM�HDN
�DN�HDO
�DO��DPHDP��DQ
�DQ��DR
�DR��DSHDS��DT
�DT�HDU
�DU��DV
�DV��DW�DW��DX
�DX��DYHDY��DZHDZ��D[�D[�HD\�D\��D]HD]�HD^HD^��D_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd��DeHDe�HDfHDf�HDgHDg��Dh�Dh�HDiHDi��DjHDj�HDkHDk�HDlHDl�HDm
�Dm�HDnHDn�HDoHDo�HDpHDp��DqHDq�HDr�Dr�HDsHDs�HDtHDt�HDuHDu�HDv�Dv�HDv�DyaHD�1�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�A�A�C�A��A��yA®A�|�A��-A�(�A��A��!A���A��hA�dZA�-A���A�K�A�A���A��A�|�A�~�A�|�A�z�A�v�A�Q�A���A��wA�t�A�O�A���A��mA��;A���A���A��7A��A�~�A�n�A�33A�A��A�  A��hA��\A�dZA��A��/A��A��uA�7LA��mA��9A�l�A�&�A��TA��uA�A�A�1A�^5A�A���A�oA�^5A��yA�x�A�O�A�oA��A��A��HA���A�ȴA�A��jA��A�ƨA�l�A���A�r�A�7LA��A���A���A��A�  A�dZA�/A�A�~�A���A��\A�C�A��A��!A�oA���A��A�`BA�  A�jA���A��uA��`A�ZA��FA�Q�A�-A��DA��A��A�~�A�r�A��PA�A�A�x�A��HA��A�A�^5A�A�A�ȴA��FA�VA��FA���A�l�A��FA���A�9XAp�A{hsAx(�Au��As�Ar�9Aq�;Aq"�Ap�\Ao�Aox�AoK�AnJAjA�Ag��AfI�Ac�TAa�;AaA^bA[�-A[��A[��A[dZAYAX9XAVr�AT��AS�mAR1'AO�AKp�AJE�AI�AHA�AG;dAFbAD�yAA�^A@n�A?�PA?�A>-A;��A:jA9p�A7�mA6VA5ƨA4�A3K�A2�/A1�FA/�A-�
A,�RA,�A*�!A'��A&�jA%��A%��A%%A#�7A"5?A!K�A VA`BAA�A"�A�HA��AM�AoAjA�;AVA��AO�A�;A�A��A�uA��Ap�A�!A��A�hA��A��A�hA
��A
A�A�AC�A�\AA?}A{AoA �H@��
@��@���@�@���@��+@�-@��T@�%@���@�S�@�C�@��R@�M�@���@�l�@��#@�S�@���@�@�x�@�p�@�X@웦@�F@��@��#@���@��@ߝ�@��@�I�@�J@�o@�^5@�/@ԃ@��
@�K�@�~�@�A�@�\)@��@�M�@�O�@̓u@�|�@��@�@�Z@Ɵ�@�Q�@ă@þw@�^5@�@��@�{@�&�@��u@�(�@���@��@�-@�@�G�@���@��@���@���@�ff@�hs@��@���@�z�@�I�@��P@�C�@���@��@��/@��9@��@�Ĝ@���@�V@��`@��@�  @�ƨ@�dZ@��@��H@��H@��H@��H@���@�ȴ@��R@�J@���@��h@���@�9X@��@��w@�|�@�+@�n�@��^@��@�O�@�7L@��@��@���@�I�@��m@�
=@�~�@�J@�@�p�@��@�r�@��
@�S�@�J@��@���@�x�@�`B@�&�@���@�Ĝ@��@��D@��D@��u@���@�z�@�Z@�9X@�1'@�1'@�1'@�  @��
@��F@�K�@�V@��@�J@�V@�^5@��#@��@���@���@��@�j@�b@��m@��;@��@��@��P@�\)@��H@��!@�^5@���@�`B@�7L@��@���@���@��D@�9X@� �@���@���@���@���@�ȴ@�
=@�+@�ȴ@�E�@�-@���@�G�@��@���@��`@���@�j@�(�@�1@��@�ƨ@��w@���@�\)@�33@�+@�33@�K�@�
=@�ȴ@��\@�v�@�E�@�$�@�@��@���@��7@��^@�hs@�/@�/@��@��@���@�9X@��
@��@��P@�|�@�S�@�
=@��R@�n�@�V@�M�@�$�@�J@��^@�p�@�?}@�V@�Ĝ@��@�bN@��;@���@�|�@�"�@���@���@�ff@�5?@���@���@���@��h@�x�@�hs@�X@�G�@�/@�H�@|�j@j0U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�A�A�C�A��A��yA®A�|�A��-A�(�A��A��!A���A��hA�dZA�-A���A�K�A�A���A��A�|�A�~�A�|�A�z�A�v�A�Q�A���A��wA�t�A�O�A���A��mA��;A���A���A��7A��A�~�A�n�A�33A�A��A�  A��hA��\A�dZA��A��/A��A��uA�7LA��mA��9A�l�A�&�A��TA��uA�A�A�1A�^5A�A���A�oA�^5A��yA�x�A�O�A�oA��A��A��HA���A�ȴA�A��jA��A�ƨA�l�A���A�r�A�7LA��A���A���A��A�  A�dZA�/A�A�~�A���A��\A�C�A��A��!A�oA���A��A�`BA�  A�jA���A��uA��`A�ZA��FA�Q�A�-A��DA��A��A�~�A�r�A��PA�A�A�x�A��HA��A�A�^5A�A�A�ȴA��FA�VA��FA���A�l�A��FA���A�9XAp�A{hsAx(�Au��As�Ar�9Aq�;Aq"�Ap�\Ao�Aox�AoK�AnJAjA�Ag��AfI�Ac�TAa�;AaA^bA[�-A[��A[��A[dZAYAX9XAVr�AT��AS�mAR1'AO�AKp�AJE�AI�AHA�AG;dAFbAD�yAA�^A@n�A?�PA?�A>-A;��A:jA9p�A7�mA6VA5ƨA4�A3K�A2�/A1�FA/�A-�
A,�RA,�A*�!A'��A&�jA%��A%��A%%A#�7A"5?A!K�A VA`BAA�A"�A�HA��AM�AoAjA�;AVA��AO�A�;A�A��A�uA��Ap�A�!A��A�hA��A��A�hA
��A
A�A�AC�A�\AA?}A{AoA �H@��
@��@���@�@���@��+@�-@��T@�%@���@�S�@�C�@��R@�M�@���@�l�@��#@�S�@���@�@�x�@�p�@�X@웦@�F@��@��#@���@��@ߝ�@��@�I�@�J@�o@�^5@�/@ԃ@��
@�K�@�~�@�A�@�\)@��@�M�@�O�@̓u@�|�@��@�@�Z@Ɵ�@�Q�@ă@þw@�^5@�@��@�{@�&�@��u@�(�@���@��@�-@�@�G�@���@��@���@���@�ff@�hs@��@���@�z�@�I�@��P@�C�@���@��@��/@��9@��@�Ĝ@���@�V@��`@��@�  @�ƨ@�dZ@��@��H@��H@��H@��H@���@�ȴ@��R@�J@���@��h@���@�9X@��@��w@�|�@�+@�n�@��^@��@�O�@�7L@��@��@���@�I�@��m@�
=@�~�@�J@�@�p�@��@�r�@��
@�S�@�J@��@���@�x�@�`B@�&�@���@�Ĝ@��@��D@��D@��u@���@�z�@�Z@�9X@�1'@�1'@�1'@�  @��
@��F@�K�@�V@��@�J@�V@�^5@��#@��@���@���@��@�j@�b@��m@��;@��@��@��P@�\)@��H@��!@�^5@���@�`B@�7L@��@���@���@��D@�9X@� �@���@���@���@���@�ȴ@�
=@�+@�ȴ@�E�@�-@���@�G�@��@���@��`@���@�j@�(�@�1@��@�ƨ@��w@���@�\)@�33@�+@�33@�K�@�
=@�ȴ@��\@�v�@�E�@�$�@�@��@���@��7@��^@�hs@�/@�/@��@��@���@�9X@��
@��@��P@�|�@�S�@�
=@��R@�n�@�V@�M�@�$�@�J@��^@�p�@�?}@�V@�Ĝ@��@�bN@��;@���@�|�@�"�@���@���@�ff@�5?@���@���@���@��h@�x�@�hs@�X@�G�@�/@�H�@|�j@j0U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B$�B#�B"�B&�BT�Bn�Bp�B�B�=B�VB�uB��B��B��B��B�{B�\B�7B�+B�B�B�B�B�B�B�B�B�B�B�B�+B�1B�1B�=B�VB�bB��B��B��B��B��B��B��B��B�B�!B�B�B�9B�FB�RB�jB�qB��BBBÖBB��BŢBǮB�
B�)B�5B�BB�ZB�fB�B�B�B�B�B�B�B�BB�B�B�B �B"�B"�B"�B �B�B�B�B�B�BPB	7B+BBB�B�B��B�3B��B� B_;B?}B(�B�B�BVB+BB��B��B�B�B�;BŢB��B�\Bw�BiyBN�BD�B&�B
��B
�)B
��B
��B
��B
�^B
�B
��B
��B
�=B
m�B
W
B
B�B
33B
,B
'�B
"�B
�B
�B
�B
hB
+B	�B	�HB	�B	ŢB	�LB	�B	��B	�1B	�7B	�=B	�7B	~�B	s�B	gmB	\)B	R�B	D�B	/B	�B	uB	PB	1B	B��B�B�mB�HB�NB�B�sB�yB�`B�;B�
B��B��BŢBBBB�^B�?B�-B�B��B��B��B��B��B��B�oB�JB�%B~�B~�B}�B|�B|�B{�B{�B}�B|�B|�B|�B|�Bz�By�Bw�Bv�Bt�Bq�Bm�Bl�Bm�Bm�Bm�BjBhsBgmBdZBaHBcTBcTBaHB^5BZBW
BQ�BN�BK�BJ�BI�BI�BI�BI�BI�BO�BVBW
BXBZBZB[#B^5BdZBjBn�Bn�Bo�Bn�Bn�Bp�Bq�Bo�Bq�Bx�Bu�Bs�Bs�Bq�Bk�BhsBiyBo�Br�Bv�Bx�B{�B�B�B�%B�+B�7B�=B�JB�hB��B��B��B��B��B��B�'B�FB�^B�jBĜBǮBȴB��B��B��B��B��B��B��B��B�B�
B�B�B�#B�)B�)B�#B�)B�HB�fB�yB�B�B��B��B��B	B	B	B	DB	hB	hB	oB	oB	uB	{B	{B	�B	�B	�B	�B	�B	"�B	(�B	)�B	+B	-B	.B	33B	8RB	9XB	:^B	:^B	;dB	;dB	;dB	@�B	B�B	E�B	G�B	I�B	M�B	S�B	T�B	T�B	T�B	T�B	T�B	W
B	XB	[#B	]/B	bNB	e`B	gmB	jB	k�B	m�B	q�B	s�B	w�B	x�B	y�B	y�B	y�B	z�B	z�B	{�B	{�B	{�B	|�B	~�B	�B	�B	�1B	�+B	�7B	�=B	�JB	�JB	�JB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�XB	�qB	�jB	�jB	�wB	��B	B	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�HB	�ZB	�ZB	�`B	�sB	�yB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 B
�B
*�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B$�B#�B"�B&�BT�Bn�Bp�B�B�=B�VB�uB��B��B��B��B�{B�\B�7B�+B�B�B�B�B�B�B�B�B�B�B�B�+B�1B�1B�=B�VB�bB��B��B��B��B��B��B��B��B�B�!B�B�B�9B�FB�RB�jB�qB��BBBÖBB��BŢBǮB�
B�)B�5B�BB�ZB�fB�B�B�B�B�B�B�B�BB�B�B�B �B"�B"�B"�B �B�B�B�B�B�BPB	7B+BBB�B�B��B�3B��B� B_;B?}B(�B�B�BVB+BB��B��B�B�B�;BŢB��B�\Bw�BiyBN�BD�B&�B
��B
�)B
��B
��B
��B
�^B
�B
��B
��B
�=B
m�B
W
B
B�B
33B
,B
'�B
"�B
�B
�B
�B
hB
+B	�B	�HB	�B	ŢB	�LB	�B	��B	�1B	�7B	�=B	�7B	~�B	s�B	gmB	\)B	R�B	D�B	/B	�B	uB	PB	1B	B��B�B�mB�HB�NB�B�sB�yB�`B�;B�
B��B��BŢBBBB�^B�?B�-B�B��B��B��B��B��B��B�oB�JB�%B~�B~�B}�B|�B|�B{�B{�B}�B|�B|�B|�B|�Bz�By�Bw�Bv�Bt�Bq�Bm�Bl�Bm�Bm�Bm�BjBhsBgmBdZBaHBcTBcTBaHB^5BZBW
BQ�BN�BK�BJ�BI�BI�BI�BI�BI�BO�BVBW
BXBZBZB[#B^5BdZBjBn�Bn�Bo�Bn�Bn�Bp�Bq�Bo�Bq�Bx�Bu�Bs�Bs�Bq�Bk�BhsBiyBo�Br�Bv�Bx�B{�B�B�B�%B�+B�7B�=B�JB�hB��B��B��B��B��B��B�'B�FB�^B�jBĜBǮBȴB��B��B��B��B��B��B��B��B�B�
B�B�B�#B�)B�)B�#B�)B�HB�fB�yB�B�B��B��B��B	B	B	B	DB	hB	hB	oB	oB	uB	{B	{B	�B	�B	�B	�B	�B	"�B	(�B	)�B	+B	-B	.B	33B	8RB	9XB	:^B	:^B	;dB	;dB	;dB	@�B	B�B	E�B	G�B	I�B	M�B	S�B	T�B	T�B	T�B	T�B	T�B	W
B	XB	[#B	]/B	bNB	e`B	gmB	jB	k�B	m�B	q�B	s�B	w�B	x�B	y�B	y�B	y�B	z�B	z�B	{�B	{�B	{�B	|�B	~�B	�B	�B	�1B	�+B	�7B	�=B	�JB	�JB	�JB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�XB	�qB	�jB	�jB	�wB	��B	B	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�HB	�ZB	�ZB	�`B	�sB	�yB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 B
�B
*�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191758                              AO  ARCAADJP                                                                    20181005191758    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191758  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191758  QCF$                G�O�G�O�G�O�8000            