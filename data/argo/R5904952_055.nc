CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:17Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190517  20181005190517  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               7A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׽$�j��1   @׽%����@1R���m�c�&�x��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      7A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C��C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  Dy�D	  D	�fD
  D
� DfD� D��D� D  D� D  Dy�D  D� D��Dy�D��D� D  D� D  D� D  D� D  Dy�D  D� D��D� D  Dy�D  D�fD  D� D��Dy�D  D� D  Dy�D  D� DfD�fD   D � D!  D!� D"  D"� D#  D#y�D#��D$� D$��D%y�D%��D&� D'fD'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6�fD7fD7� D8  D8� D8��D9� D:  D:� D;  D;�fD<fD<� D<��D=� D=��D>� D?fD?� D@fD@� DA  DA� DB  DB� DB��DCy�DC��DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM�fDN  DNy�DOfDO� DO��DP� DQ  DQy�DR  DR� DS  DS� DT  DTy�DT��DU� DV  DV� DWfDW� DW��DX� DYfDY�fDZ  DZ� D[  D[y�D\  D\� D\��D]� D^  D^� D^��D_y�D`  D`� D`��Day�Db  Db�fDcfDc�fDdfDd� De  De� Df  Df�fDgfDgy�Dg��Dh� Di  Diy�Di��Djy�Dj��Dky�DlfDl� Dm  Dm� Dn  Dn�fDo  Do� DpfDp�fDqfDq� Dr  Dr� Dr��Ds� Dt  Dt� Dt��Du� DvfDv� Dw  Dw� Dw� Dy��D�M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @J�H@���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�\)A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B)z�B1{B8�BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B�W
B�W
BĊ=BȊ=B�W
B�W
BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECD^�CF^�CHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECx+�CzEC|^�C~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�/\C�/\C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C��C�"�C�/\C�"�C�/\C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD�D�HDHD��D	HD	��D
HD
�HD�D�HD
�D�HDHD�HDHD��DHD�HD
�D��D
�D�HDHD�HDHD�HDHD�HDHD��DHD�HD
�D�HDHD��DHD��DHD�HD
�D��DHD�HDHD��DHD�HD�D��D HD �HD!HD!�HD"HD"�HD#HD#��D$
�D$�HD%
�D%��D&
�D&�HD'�D'�HD(HD(��D)HD)�HD*HD*�HD+HD+�HD,HD,�HD-�D-�HD.HD.��D/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4��D5HD5�HD6HD6��D7�D7�HD8HD8�HD9
�D9�HD:HD:�HD;HD;��D<�D<�HD=
�D=�HD>
�D>�HD?�D?�HD@�D@�HDAHDA�HDBHDB�HDC
�DC��DD
�DD�HDEHDE�HDFHDF�HDGHDG�HDHHDH��DIHDI�HDJHDJ�HDKHDK�HDLHDL�HDM�DM��DNHDN��DO�DO�HDP
�DP�HDQHDQ��DRHDR�HDSHDS�HDTHDT��DU
�DU�HDVHDV�HDW�DW�HDX
�DX�HDY�DY��DZHDZ�HD[HD[��D\HD\�HD]
�D]�HD^HD^�HD_
�D_��D`HD`�HDa
�Da��DbHDb��Dc�Dc��Dd�Dd�HDeHDe�HDfHDf��Dg�Dg��Dh
�Dh�HDiHDi��Dj
�Dj��Dk
�Dk��Dl�Dl�HDmHDm�HDnHDn��DoHDo�HDp�Dp��Dq�Dq�HDrHDr�HDs
�Ds�HDtHDt�HDu
�Du�HDv�Dv�HDwHDw�HDw�HDy�D�Vg111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�9XA�9XA�A�A�G�A�E�A�I�A�M�A�M�A�M�A�Q�A�O�A�I�A�G�A�M�A�
=A���A���A�ƨAٸRAٓuA�G�A��A�?}A��A��A��HA��Aו�A���A��AՁAՁAՑhA�`BA�ȴA��AӬA���A��A�hsAЋDA�
=A�n�A�oAͲ-A͇+A���AˍPA���A�jA�%A�E�A���A�x�A�/Aź^AċDA���A��HA���A��A��mA�&�A���A�A���A�JA�-A�l�A���A���A��A���A��A���A���A���A��^A��wA�t�A���A��!A��7A�JA���A���A���A��A� �A��uA�l�A��HA��A���A��9A�A�A���A���A�A��A��A�I�A�p�A��#A���A�n�A��HA���A��A�A��A��!A��RA~z�Ay�;AuoAsdZAq/Akt�Aex�Ac�Aa&�A_
=A\�RA[�AZ��AX��AX��AW�wAVz�AU��AS�AO�AL��AK%AI��AIC�AI&�AH�AHn�AF��AE�FAE"�ACAAXA@�!A>�!A>�A=
=A;|�A9/A7��A61A4��A1��A/\)A.bA-7LA+�wA(ZA'&�A%��A#C�A#
=A"n�A E�AffA��A�A�;AoA�A��AȴA��Av�AJAbNAA��A33A=qAS�A-A+A�RA�!A�\AQ�AXA+A�A�/A�uAn�A��A �AƨA��Ax�AE�A�
A|�A�DA
�`A	��A33A�A�9A�+A�A�#AE�@�~�@�z�@��7@�O�@��@�x�@�u@�j@�ff@���@�(�@�@�\)@�5?@�+@�V@�7@�%@��@�J@�V@�D@���@�r�@�n�@陚@��@��y@���@�j@◍@���@�&�@��u@��
@߅@���@�b@�33@��y@��@ݲ-@܋D@۶F@ڧ�@�M�@�M�@�V@�M�@�M�@�M�@�{@ف@���@��@ָR@���@Չ7@�p�@�(�@Ӿw@�;d@��y@��@�;d@���@��@�j@�Q�@�1@���@�dZ@��@�~�@·+@�V@��@�{@�l�@�-@�G�@�ƨ@��-@�z�@��@�&�@���@���@���@���@�X@���@��@��7@�bN@��P@��@�`B@��j@���@�|�@��w@��@���@�;d@�+@�o@��H@�ȴ@�-@��#@�x�@���@��9@��m@�\)@�"�@���@��T@��!@��@��+@�$�@���@��9@���@��!@��+@�^5@���@��^@���@���@�O�@�Z@��;@��@��@��@��
@�\)@�M�@��-@��T@�@�$�@�$�@�$�@��h@��/@�bN@� �@���@��H@�^5@�=q@��@���@�`B@��@��@��9@���@�\)@�K�@�+@���@���@�@���@�7L@���@��u@�r�@�b@���@��P@�S�@�33@�
=@��+@�V@�E�@�$�@�{@���@��@���@��@��^@��#@��@��+@�"�@��@�@��+@�5?@��@���@�p�@�X@�V@��9@� �@��F@�;d@�+@�o@��R@�~�@�ff@�=q@�-@�@��@��#@�@�&�@��u@�A�@�9X@� �@��@���@���@�+@�ȴ@�v�@�J@���@��h@���@�r�@�I�@� �@��;@��P@�"�@��H@�ȴ@��!@��\@�=q@�@��h@�hs@�?}@�/@��/@�z�@�(�@�  @�ƨ@��P@�l�@�S�@�+@��@��R@��+@�=q@���@�J@���@��h@�?}@���@���@��@�O�@�V@�Z@�9X@�Z@�;d@�ȴ@��H@���@�^5@��!@��H@���@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�;dA�9XA�9XA�A�A�G�A�E�A�I�A�M�A�M�A�M�A�Q�A�O�A�I�A�G�A�M�A�
=A���A���A�ƨAٸRAٓuA�G�A��A�?}A��A��A��HA��Aו�A���A��AՁAՁAՑhA�`BA�ȴA��AӬA���A��A�hsAЋDA�
=A�n�A�oAͲ-A͇+A���AˍPA���A�jA�%A�E�A���A�x�A�/Aź^AċDA���A��HA���A��A��mA�&�A���A�A���A�JA�-A�l�A���A���A��A���A��A���A���A���A��^A��wA�t�A���A��!A��7A�JA���A���A���A��A� �A��uA�l�A��HA��A���A��9A�A�A���A���A�A��A��A�I�A�p�A��#A���A�n�A��HA���A��A�A��A��!A��RA~z�Ay�;AuoAsdZAq/Akt�Aex�Ac�Aa&�A_
=A\�RA[�AZ��AX��AX��AW�wAVz�AU��AS�AO�AL��AK%AI��AIC�AI&�AH�AHn�AF��AE�FAE"�ACAAXA@�!A>�!A>�A=
=A;|�A9/A7��A61A4��A1��A/\)A.bA-7LA+�wA(ZA'&�A%��A#C�A#
=A"n�A E�AffA��A�A�;AoA�A��AȴA��Av�AJAbNAA��A33A=qAS�A-A+A�RA�!A�\AQ�AXA+A�A�/A�uAn�A��A �AƨA��Ax�AE�A�
A|�A�DA
�`A	��A33A�A�9A�+A�A�#AE�@�~�@�z�@��7@�O�@��@�x�@�u@�j@�ff@���@�(�@�@�\)@�5?@�+@�V@�7@�%@��@�J@�V@�D@���@�r�@�n�@陚@��@��y@���@�j@◍@���@�&�@��u@��
@߅@���@�b@�33@��y@��@ݲ-@܋D@۶F@ڧ�@�M�@�M�@�V@�M�@�M�@�M�@�{@ف@���@��@ָR@���@Չ7@�p�@�(�@Ӿw@�;d@��y@��@�;d@���@��@�j@�Q�@�1@���@�dZ@��@�~�@·+@�V@��@�{@�l�@�-@�G�@�ƨ@��-@�z�@��@�&�@���@���@���@���@�X@���@��@��7@�bN@��P@��@�`B@��j@���@�|�@��w@��@���@�;d@�+@�o@��H@�ȴ@�-@��#@�x�@���@��9@��m@�\)@�"�@���@��T@��!@��@��+@�$�@���@��9@���@��!@��+@�^5@���@��^@���@���@�O�@�Z@��;@��@��@��@��
@�\)@�M�@��-@��T@�@�$�@�$�@�$�@��h@��/@�bN@� �@���@��H@�^5@�=q@��@���@�`B@��@��@��9@���@�\)@�K�@�+@���@���@�@���@�7L@���@��u@�r�@�b@���@��P@�S�@�33@�
=@��+@�V@�E�@�$�@�{@���@��@���@��@��^@��#@��@��+@�"�@��@�@��+@�5?@��@���@�p�@�X@�V@��9@� �@��F@�;d@�+@�o@��R@�~�@�ff@�=q@�-@�@��@��#@�@�&�@��u@�A�@�9X@� �@��@���@���@�+@�ȴ@�v�@�J@���@��h@���@�r�@�I�@� �@��;@��P@�"�@��H@�ȴ@��!@��\@�=q@�@��h@�hs@�?}@�/@��/@�z�@�(�@�  @�ƨ@��P@�l�@�S�@�+@��@��R@��+@�=q@���@�J@���@��h@�?}@���@���@��@�O�@�V@�Z@�9X@�Z@�;d@�ȴ@��H@���@�^5@��!@��H@���@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
gmB
�wB
��B
��B
��B
�B
��B
��B
ĜB
�XB
�FB
�jB
��B
��B
ÖB
ÖB
ɺB
�yB
��BB%B  B
��B
��BB&�BK�B_;BiyBp�B�B�1B�hB��B��B�^BÖBɺB��B�B�B�B�HB�B��B1BhB\BVBhB@�B?}BaHBjBgmB\)BR�BO�BE�BK�BK�B�B��B��B^5BP�B`BBcTBq�Bw�Bz�By�Br�Bq�BhsB^5BVBE�B1'B#�BB�B�NBɺBB��B��B��B�PBn�B\)B �B
�NB
�B
v�B
B�B
1'B
�B

=B	��B	�ZB	��B	�B	��B	�DB	l�B	W
B	L�B	>wB	0!B	�B	uB	�B	�B	�B	�B	�B	�B	DB��B�B�B�B�sB�mB�fB�TB�BB�)B�B��B��B��B��BȴBƨBĜB��B�jB�^B�RB�^B�XB�RB�FB�3B�!B�B�B�B��B��B�B�!B�3B�9B�LB�XB�RB�XB�jB�qBĜB��B�B�#B�BB�/B�#B�ZB�fB�NB�NB�TB�TB�TB�NB�HB�BB�BB�BB�sB�B�ZB�B	B	�B	1'B	1'B	.B	(�B	�B	�B	B	%B�B�B�yB�B��B�NBɺB��BƨBĜBƨBǮB��B��B�B�HB�BB�ZB�yB��B��B��B	  B	B	B	oB	(�B	.B	8RB	9XB	;dB	=qB	;dB	:^B	=qB	8RB	8RB	;dB	>wB	=qB	F�B	J�B	M�B	Q�B	VB	]/B	cTB	bNB	dZB	ffB	gmB	iyB	jB	l�B	m�B	n�B	r�B	t�B	t�B	t�B	q�B	p�B	q�B	q�B	t�B	u�B	u�B	v�B	w�B	|�B	}�B	|�B	|�B	}�B	~�B	� B	� B	~�B	~�B	~�B	y�B	q�B	m�B	ffB	bNB	`BB	\)B	ZB	\)B	ffB	k�B	k�B	m�B	jB	gmB	jB	u�B	{�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�3B	�9B	�9B	�9B	�3B	�?B	�FB	�RB	�wB	�}B	�wB	�wB	�wB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�HB	�NB	�TB	�ZB	�fB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
%B
+B
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
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
JB
JB
JB
\B
bB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
qB
)222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
gmB
�wB
��B
��B
��B
�B
��B
��B
ĜB
�XB
�FB
�jB
��B
��B
ÖB
ÖB
ɺB
�yB
��BB%B  B
��B
��BB&�BK�B_;BiyBp�B�B�1B�hB��B��B�^BÖBɺB��B�B�B�B�HB�B��B1BhB\BVBhB@�B?}BaHBjBgmB\)BR�BO�BE�BK�BK�B�B��B��B^5BP�B`BBcTBq�Bw�Bz�By�Br�Bq�BhsB^5BVBE�B1'B#�BB�B�NBɺBB��B��B��B�PBn�B\)B �B
�NB
�B
v�B
B�B
1'B
�B

=B	��B	�ZB	��B	�B	��B	�DB	l�B	W
B	L�B	>wB	0!B	�B	uB	�B	�B	�B	�B	�B	�B	DB��B�B�B�B�sB�mB�fB�TB�BB�)B�B��B��B��B��BȴBƨBĜB��B�jB�^B�RB�^B�XB�RB�FB�3B�!B�B�B�B��B��B�B�!B�3B�9B�LB�XB�RB�XB�jB�qBĜB��B�B�#B�BB�/B�#B�ZB�fB�NB�NB�TB�TB�TB�NB�HB�BB�BB�BB�sB�B�ZB�B	B	�B	1'B	1'B	.B	(�B	�B	�B	B	%B�B�B�yB�B��B�NBɺB��BƨBĜBƨBǮB��B��B�B�HB�BB�ZB�yB��B��B��B	  B	B	B	oB	(�B	.B	8RB	9XB	;dB	=qB	;dB	:^B	=qB	8RB	8RB	;dB	>wB	=qB	F�B	J�B	M�B	Q�B	VB	]/B	cTB	bNB	dZB	ffB	gmB	iyB	jB	l�B	m�B	n�B	r�B	t�B	t�B	t�B	q�B	p�B	q�B	q�B	t�B	u�B	u�B	v�B	w�B	|�B	}�B	|�B	|�B	}�B	~�B	� B	� B	~�B	~�B	~�B	y�B	q�B	m�B	ffB	bNB	`BB	\)B	ZB	\)B	ffB	k�B	k�B	m�B	jB	gmB	jB	u�B	{�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�3B	�9B	�9B	�9B	�3B	�?B	�FB	�RB	�wB	�}B	�wB	�wB	�wB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�HB	�NB	�TB	�ZB	�fB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
%B
+B
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
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
JB
JB
JB
\B
bB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
qB
)222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190517                              AO  ARCAADJP                                                                    20181005190517    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190517  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190517  QCF$                G�O�G�O�G�O�8000            