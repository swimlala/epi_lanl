CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:52Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190552  20181005190552  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)Z�1   @��*`��@1�� ě��c�n��O�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B��B  B  B ffB(  B0  B8  B?��BH  BPffBX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C  C�fC�fC�fC
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C��C��C��C�  C��3C��3C�  C�  C��C��C�  C��3C��3C�  C��C��C��C��C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  Dy�D��D� DfD� D  D�fD  D� D  Dy�D��D� D  D� DfD� D��Dy�D��D� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D��Dy�D  D� D   D � D!  D!� D!��D"� D#fD#� D#��D$� D%  D%� D&fD&�fD'  D'y�D'��D(� D)  D)� D*  D*y�D+  D+� D,fD,�fD-  D-y�D.  D.�fD/  D/� D0  D0� D1  D1y�D1��D2y�D2��D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D8��D9y�D:  D:� D;  D;� D<  D<� D<��D=� D>  D>�fD?  D?� D@  D@�fDAfDA�fDB  DB�fDC  DCy�DD  DD� DD��DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DMfDM�fDN  DN� DO  DO� DP  DP�fDP��DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DY��DZy�D[  D[� D\  D\y�D]  D]�fD^  D^� D_  D_� D`  D`� D`��Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� De��Df� DgfDg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dmy�Dm��Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dvy�Dw  Dw� Dw��Dy��D�)�D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Dz�@��
@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B �B�B{B{B!z�B){B1{B9{B@�BI{BQz�BY{Ba{Bi{Bq{By{B��pB��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B�W
C +�CEC+�C+�C+�C
ECEC^�CECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:^�C<^�C>EC@ECBECDECFECHECJECLECNECPECRECTECVECX+�CZEC\^�C^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC��C��C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C��C��C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�/\C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�/\C�/\C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�/\C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�/\C�/\C�/\C�"�C��C��C�"�C�"�C�/\C�/\C�"�C��C��C�"�C�/\C�/\C�/\C�/\C�"�C�"�C�"�C�"�D HD �HDHD�HD�D�HDHD�HDHD�HDHD�HDHD�HD
�D�HDHD�HD	HD	�HD
HD
�HDHD�HDHD��D
�D�HD�D�HDHD��DHD�HDHD��D
�D�HDHD�HD�D�HD
�D��D
�D�HDHD�HDHD�HDHD��DHD�HDHD�HDHD��DHD�HD
�D��DHD�HD HD �HD!HD!�HD"
�D"�HD#�D#�HD$
�D$�HD%HD%�HD&�D&��D'HD'��D(
�D(�HD)HD)�HD*HD*��D+HD+�HD,�D,��D-HD-��D.HD.��D/HD/�HD0HD0�HD1HD1��D2
�D2��D3
�D3�HD4HD4�HD5HD5�HD6HD6�HD7HD7��D8HD8�HD9
�D9��D:HD:�HD;HD;�HD<HD<�HD=
�D=�HD>HD>��D?HD?�HD@HD@��DA�DA��DBHDB��DCHDC��DDHDD�HDE
�DE�HDFHDF��DGHDG�HDHHDH�HDIHDI�HDJHDJ�HDK�DK�HDLHDL�HDM�DM��DNHDN�HDOHDO�HDPHDP��DQ
�DQ�HDRHDR�HDS�DS�HDTHDT�HDUHDU�HDVHDV�HDWHDW��DXHDX�HDYHDY�HDZ
�DZ��D[HD[�HD\HD\��D]HD]��D^HD^�HD_HD_�HD`HD`�HDa
�Da�HDbHDb��DcHDc�HDdHDd�HDeHDe�HDf
�Df�HDg�Dg��DhHDh�HDiHDi�HDjHDj�HDkHDk��DlHDl�HDmHDm��Dn
�Dn�HDoHDo�HDpHDp�HDq
�Dq�HDrHDr�HDsHDs�HDt�Dt�HDuHDu�HDvHDv��DwHDw�HDw�Dy�D�2�D�Ϯ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A�A���A�ȴA�A��#A��/A��A��HA��HA��A��A��`A��`A�A�A�A�%A�1A�%A�%A�%A�%A�%A�%A�1A�A�A�A�A�A�  A�  A�A�A�%A�
=A�
=A�VA�VA��mA���A���AȰ!AȰ!AȲ-AȲ-Aȴ9AȸRAȶFA���A���A���A�;dAǧ�Aǣ�Aǧ�AǋDA�z�A�r�A�hsA�G�A���AƁA�dZA���Ať�A�G�A�Aď\A�1'A��`A×�A�K�A7A�E�A�M�A�|�A®A�A�A��wA��!A�I�A�A�7LA���A��`A�ZA���A�r�A�
=A��RA�-A�|�A�r�A��A�x�A�bA���A���A���A��A�^5A��A�=qA��TA��A���A�bA��A��`A���A��A�9XA�A�A�~�A�7LA�ƨA��^A��-A��A��A�ffA�/A|ZAwoAt�yAq�FAl�9AjJAe��Ac
=A_33A]�A\AX$�AU��AT�AQ��AO\)AL��AJv�AG��AF�+AD�HAC&�A@�A=�A:�RA:$�A933A6�A4�DA3�-A.9XA,I�A*��A)�^A(-A&�RA$��A#x�A"��A!�#A!C�A ��A �yA �9A�mAJAn�AXA�AbNA�A-A�A��A+A�\AJAA+A=qA�+A��A�RA=qA��AJAhsA�wA��Ar�AQ�AO�A	O�A�\A"�AQ�An�AffA$�A��AAjA��A��A ��A �\A E�@��@�v�@���@���@��@���@��T@�I�@�33@�?}@���@�bN@�@�+@�E�@��-@�I�@�@�!@�@�r�@�  @��@�+@�p�@�bN@�|�@�"�@��@�E�@�{@�7@�|�@�M�@�x�@��m@�;d@ڇ+@ٲ-@���@؋D@��
@ו�@�
=@ՙ�@��;@�-@��@д9@�5?@ӝ�@�5?@��@�&�@أ�@�Q�@�9X@�  @�(�@�Q�@ו�@�@�o@���@�n�@�{@Ցh@�1'@�t�@�n�@�r�@�G�@˥�@�33@�-@ȣ�@�z�@�  @Ǿw@��/@���@�5?@�v�@ʏ\@ʗ�@��T@�Z@�1@��m@Ǯ@�
=@�@�O�@��;@¸R@�G�@�%@�ƨ@�33@�{@�hs@�&�@�%@���@�Z@��@�r�@�r�@�9X@��
@�l�@��@���@��h@�Z@�ƨ@��;@�33@�@�G�@���@�Q�@��;@�ƨ@���@�l�@��H@���@�G�@��/@��D@�  @�C�@�+@��@��y@��R@���@���@��\@�ff@�$�@��@��@�&�@��`@�Q�@�  @��m@��
@��P@�K�@��@���@��+@�5?@�{@��T@�/@��9@�A�@��;@���@�o@�-@�{@�@���@�X@�/@���@��u@�1'@��@��m@��m@��;@��;@��@�|�@�\)@�;d@�+@�o@���@��@�@��h@�?}@���@�j@�I�@�A�@� �@���@�
=@�v�@�ff@�E�@���@���@��7@�V@��@��u@�r�@�(�@��;@��F@��@�|�@�t�@�C�@��y@�V@�{@���@�O�@�/@���@���@���@��@�Q�@�  @��;@��F@��@�+@��H@��!@�v�@�M�@���@�x�@�hs@�&�@��j@��u@�j@��@�|�@�S�@�
=@��H@��R@�n�@�$�@�J@���@�hs@�?}@��@��@��@�  @���@��w@�@���@��!@�M�@�{@���@���@��7@�&�@�Ĝ@��D@��@��@��;@��@�"�@���@���@�~�@�5?@��^@�x�@�X@�G�@��@��$@~�8@i+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A�A���A�ȴA�A��#A��/A��A��HA��HA��A��A��`A��`A�A�A�A�%A�1A�%A�%A�%A�%A�%A�%A�1A�A�A�A�A�A�  A�  A�A�A�%A�
=A�
=A�VA�VA��mA���A���AȰ!AȰ!AȲ-AȲ-Aȴ9AȸRAȶFA���A���A���A�;dAǧ�Aǣ�Aǧ�AǋDA�z�A�r�A�hsA�G�A���AƁA�dZA���Ať�A�G�A�Aď\A�1'A��`A×�A�K�A7A�E�A�M�A�|�A®A�A�A��wA��!A�I�A�A�7LA���A��`A�ZA���A�r�A�
=A��RA�-A�|�A�r�A��A�x�A�bA���A���A���A��A�^5A��A�=qA��TA��A���A�bA��A��`A���A��A�9XA�A�A�~�A�7LA�ƨA��^A��-A��A��A�ffA�/A|ZAwoAt�yAq�FAl�9AjJAe��Ac
=A_33A]�A\AX$�AU��AT�AQ��AO\)AL��AJv�AG��AF�+AD�HAC&�A@�A=�A:�RA:$�A933A6�A4�DA3�-A.9XA,I�A*��A)�^A(-A&�RA$��A#x�A"��A!�#A!C�A ��A �yA �9A�mAJAn�AXA�AbNA�A-A�A��A+A�\AJAA+A=qA�+A��A�RA=qA��AJAhsA�wA��Ar�AQ�AO�A	O�A�\A"�AQ�An�AffA$�A��AAjA��A��A ��A �\A E�@��@�v�@���@���@��@���@��T@�I�@�33@�?}@���@�bN@�@�+@�E�@��-@�I�@�@�!@�@�r�@�  @��@�+@�p�@�bN@�|�@�"�@��@�E�@�{@�7@�|�@�M�@�x�@��m@�;d@ڇ+@ٲ-@���@؋D@��
@ו�@�
=@ՙ�@��;@�-@��@д9@�5?@ӝ�@�5?@��@�&�@أ�@�Q�@�9X@�  @�(�@�Q�@ו�@�@�o@���@�n�@�{@Ցh@�1'@�t�@�n�@�r�@�G�@˥�@�33@�-@ȣ�@�z�@�  @Ǿw@��/@���@�5?@�v�@ʏ\@ʗ�@��T@�Z@�1@��m@Ǯ@�
=@�@�O�@��;@¸R@�G�@�%@�ƨ@�33@�{@�hs@�&�@�%@���@�Z@��@�r�@�r�@�9X@��
@�l�@��@���@��h@�Z@�ƨ@��;@�33@�@�G�@���@�Q�@��;@�ƨ@���@�l�@��H@���@�G�@��/@��D@�  @�C�@�+@��@��y@��R@���@���@��\@�ff@�$�@��@��@�&�@��`@�Q�@�  @��m@��
@��P@�K�@��@���@��+@�5?@�{@��T@�/@��9@�A�@��;@���@�o@�-@�{@�@���@�X@�/@���@��u@�1'@��@��m@��m@��;@��;@��@�|�@�\)@�;d@�+@�o@���@��@�@��h@�?}@���@�j@�I�@�A�@� �@���@�
=@�v�@�ff@�E�@���@���@��7@�V@��@��u@�r�@�(�@��;@��F@��@�|�@�t�@�C�@��y@�V@�{@���@�O�@�/@���@���@���@��@�Q�@�  @��;@��F@��@�+@��H@��!@�v�@�M�@���@�x�@�hs@�&�@��j@��u@�j@��@�|�@�S�@�
=@��H@��R@�n�@�$�@�J@���@�hs@�?}@��@��@��@�  @���@��w@�@���@��!@�M�@�{@���@���@��7@�&�@�Ĝ@��D@��@��@��;@��@�"�@���@���@�~�@�5?@��^@�x�@�X@�G�@��@��$@~�8@i+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�`B
�`B
�fB
�fB
�fB
�fB
�`B
�fB
�fB
�`B
�fB
�mB
�yB
�B
�mB
�mB
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
�B
�B
�B
�B
�B
��B
��B
��B
��BBB%B1B	7BDBJB\BoB�B�B�BS�Be`Bn�Bt�Bu�Bu�Br�Bo�Bt�Bu�Bz�B�B�=B��B��B�9B�dBÖB��B�#B��B1B�B1'B2-BPBB
=B�B/B?}B@�BC�BC�BB�B@�B=qB:^B8RB7LB7LB6FB2-B$�B{B��B�#BƨB�B�bB~�BgmBcTB\)BZBYBQ�B.B�B  B
�B
�B
��B
y�B
hsB
]/B
M�B
33B
�B	��B	��B	�wB	��B	�\B	|�B	e`B	R�B	?}B	7LB	0!B	�B	B��B�B�BB�B��B�NB�/B�B��B��B��B��BǮBŢBB�wB�LB��B��B��B��B�{B�hB�VB�DB�7B�7B�7B�1B�+B�B� Bx�Bu�Bs�Br�Bq�Br�Br�Br�Bq�Br�Bq�Bq�Bp�Bo�Bs�Br�Br�Bt�Bs�Bz�B�hB�hB�+B�DB��B��B��B��B�{B�uB��B��B��B�B�LB��B��BĜBŢBŢBƨBƨBǮBƨBŢBȴBǮBǮBǮBȴBǮBǮBɺB��B��B��B��B��B��B��B��B��B�B�
B�B�#B�/B�;B�HB�TB�`B�sB�B�B�B��B��B��B��B��B	B	B	B	1B	1B	
=B	VB	uB	�B	�B	#�B	1'B	>wB	gmB	hsB	dZB	k�B	jB	k�B	m�B	u�B	~�B	~�B	�B	�B	�+B	�1B	�1B	�+B	�%B	�B	� B	x�B	t�B	s�B	s�B	t�B	s�B	t�B	u�B	x�B	�B	�7B	�DB	�JB	�PB	�bB	�hB	�{B	��B	��B	�{B	�{B	��B	��B	�hB	�VB	�=B	�DB	�1B	�+B	�%B	�B	�B	�%B	�=B	�VB	�VB	�hB	��B	��B	�{B	�uB	�uB	��B	��B	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�LB	�RB	�jB	�wB	�wB	�wB	�}B	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�5B	�;B	�;B	�;B	�HB	�TB	�TB	�TB	�TB	�TB	�ZB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
	7B

=B
DB
DB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
hB
hB
bB
hB
oB
uB
uB
uB
{B
2B
 'B
1�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�`B
�`B
�fB
�fB
�fB
�fB
�`B
�fB
�fB
�`B
�fB
�mB
�yB
�B
�mB
�mB
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
�B
�B
�B
�B
�B
��B
��B
��B
��BBB%B1B	7BDBJB\BoB�B�B�BS�Be`Bn�Bt�Bu�Bu�Br�Bo�Bt�Bu�Bz�B�B�=B��B��B�9B�dBÖB��B�#B��B1B�B1'B2-BPBB
=B�B/B?}B@�BC�BC�BB�B@�B=qB:^B8RB7LB7LB6FB2-B$�B{B��B�#BƨB�B�bB~�BgmBcTB\)BZBYBQ�B.B�B  B
�B
�B
��B
y�B
hsB
]/B
M�B
33B
�B	��B	��B	�wB	��B	�\B	|�B	e`B	R�B	?}B	7LB	0!B	�B	B��B�B�BB�B��B�NB�/B�B��B��B��B��BǮBŢBB�wB�LB��B��B��B��B�{B�hB�VB�DB�7B�7B�7B�1B�+B�B� Bx�Bu�Bs�Br�Bq�Br�Br�Br�Bq�Br�Bq�Bq�Bp�Bo�Bs�Br�Br�Bt�Bs�Bz�B�hB�hB�+B�DB��B��B��B��B�{B�uB��B��B��B�B�LB��B��BĜBŢBŢBƨBƨBǮBƨBŢBȴBǮBǮBǮBȴBǮBǮBɺB��B��B��B��B��B��B��B��B��B�B�
B�B�#B�/B�;B�HB�TB�`B�sB�B�B�B��B��B��B��B��B	B	B	B	1B	1B	
=B	VB	uB	�B	�B	#�B	1'B	>wB	gmB	hsB	dZB	k�B	jB	k�B	m�B	u�B	~�B	~�B	�B	�B	�+B	�1B	�1B	�+B	�%B	�B	� B	x�B	t�B	s�B	s�B	t�B	s�B	t�B	u�B	x�B	�B	�7B	�DB	�JB	�PB	�bB	�hB	�{B	��B	��B	�{B	�{B	��B	��B	�hB	�VB	�=B	�DB	�1B	�+B	�%B	�B	�B	�%B	�=B	�VB	�VB	�hB	��B	��B	�{B	�uB	�uB	��B	��B	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�LB	�RB	�jB	�wB	�wB	�wB	�}B	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�5B	�;B	�;B	�;B	�HB	�TB	�TB	�TB	�TB	�TB	�ZB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
	7B

=B
DB
DB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
hB
hB
bB
hB
oB
uB
uB
uB
{B
2B
 'B
1�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190552                              AO  ARCAADJP                                                                    20181005190552    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190552  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190552  QCF$                G�O�G�O�G�O�8000            