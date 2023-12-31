CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:22Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140822  20181024140822  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               bA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$�V)�1   @��%ffy@3�9XbN�c��S���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      bA   A   A   @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX��B_��Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D� D  D� D  D� D  D� D  D� D  D�fDfD� D  Dy�D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DHfDH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DOy�DP  DP� DQ  DQ� DQ��DRy�DR��DS� DS��DT� DU  DU� DU��DVy�DV��DW� DXfDX� DX��DY� DZ  DZ�fD[  D[y�D[��D\� D]  D]� D]��D^y�D_  D_�fD`  D`� Da  Da�fDb  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDrfDr� Ds  Dsy�Dt  Dt� Du  Duy�Du��Dv� Dw  Dwy�DwٚDy� D�G
D�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�=q@ȣ�AQ�A"�RADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{Bz�B{B!{B){B1{B9{BA{BI{BQ{BY�HB`�Bi{Bp�By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=B�W
BԊ=B؊=BܽpB��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<+�C>EC@^�CBECDECFECHECJECLECNECPECR^�CTECVECXECZEC\^�C^EC`ECbECdECfEChECj^�ClECnECpECrECtECv+�CxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C��C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HD�HDHD�HDHD�HDHD�HDHD�HDHD��D�D�HDHD��DHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$��D%HD%�HD&HD&�HD'HD'�HD(HD(�HD)
�D)�HD*HD*��D+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:��D;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDE
�DE�HDFHDF�HDGHDG�HDH�DH�HDIHDI�HDJ
�DJ�HDKHDK�HDLHDL�HDMHDM�HDN
�DN�HDOHDO��DPHDP�HDQHDQ�HDR
�DR��DS
�DS�HDT
�DT�HDUHDU�HDV
�DV��DW
�DW�HDX�DX�HDY
�DY�HDZHDZ��D[HD[��D\
�D\�HD]HD]�HD^
�D^��D_HD_��D`HD`�HDaHDa��DbHDb�HDc�Dc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi��DjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq��Dr�Dr�HDsHDs��DtHDt�HDuHDu��Dv
�Dv�HDwHDw��Dw��Dy�HD�O�D�"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�S�A�VA�VA�S�A�VA�VA�XA�XA�ZA�S�A�O�A�G�A�C�A�9XA�33A�33A�1'A�+A��A���A�7LA��/A��A�XAأ�A՟�A���A�%AΏ\A�ZA̋DAʥ�A�ȴA�t�A�ƨA��AǅA��#Aď\A×�A�z�A���A��#A�t�A�
=A��PA��A���A�bA��A�hsA�^5A��PA�r�A��HA��A�+A�I�A�G�A�+A�bA�A�A��mA�9XA�1'A�\)A�|�A�l�A��A���A�-A�n�A�9XA�I�A��;A��hA�A�A�JA���A�JA��HA��A��A�bA�jA��HA��#A�O�A���A�`BA�  A�ZA��hA�33A�ƨA��A�\)A���A�oA��HA�VA�G�A�JA7LA};dA|�HA|ffAy��Aw��Ao�Am��Al9XAj�Ai�Ah��Af�yAe�FAdr�Ac��Ab��Aa�A_&�A]33A[;dAX�AV�HAR��AO��AN�yAN  ALr�AJ(�AGƨAD�AAƨA@�A>��A=/A;�A9�A81A5��A4�A2n�A0��A/�^A/�A.�!A.-A-�A+?}A*jA)O�A&M�A%"�A$�A$v�A"�yA��AO�A&�AVA��Az�A1AffA�hA��A�PA�RAXA��A��A�A�AȴA�9A�+A��AƨA�hAS�A
=AA�A��A��A
=qA	�hAĜA~�AZA5?A��A�AS�A (�@�S�@�O�@�dZ@�5?@��@�t�@���@���@�(�@�t�@�@���@땁@�V@�@�9X@�dZ@�n�@�O�@�1'@� �@�1@�@�t�@�S�@�@��@�Q�@���@ڏ\@�ff@�E�@��@ى7@�V@�Ĝ@�Z@��y@�n�@��T@�X@��/@�r�@�@ϥ�@���@͉7@́@̛�@�j@�Z@�Z@�Q�@�Q�@�I�@�9X@�b@�v�@��@�%@�p�@���@�C�@�;d@�+@�"�@��H@���@���@�Z@�t�@�E�@��@���@���@�%@��@�j@���@�K�@��w@�"�@�M�@�$�@�J@���@��@�
=@�33@�;d@���@�j@�z�@��u@�9X@��@�n�@�@�V@�^5@�=q@��@��@�9X@��
@��m@��w@�\)@�;d@�
=@��@�{@�?}@��@��@���@��`@��j@���@�r�@���@�ƨ@�ƨ@�ƨ@�t�@�@��@���@�ff@��@���@��h@�p�@��@��@�Z@��@��w@���@�t�@�33@�+@��@���@��@���@�p�@�7L@�Ĝ@��D@�Q�@�  @��P@�;d@��@��\@�5?@�@�hs@�G�@�&�@��@�V@��@�r�@� �@�ƨ@�K�@��\@�V@�ff@�^5@��@���@�J@�$�@�$�@�J@���@���@��-@��@���@�b@�b@��m@��
@��
@��@�t�@�+@�@���@�M�@��-@�p�@���@��9@�1@���@�^5@�x�@���@��j@�j@�A�@�b@��
@��F@��@���@�dZ@�33@��y@���@�~�@�M�@�5?@��@���@�?}@�&�@�Ĝ@�9X@� �@��@�1@��@�ƨ@���@��P@�|�@�t�@�K�@���@���@��+@�v�@�ff@�-@���@��T@��^@�p�@�/@�r�@�(�@���@�ƨ@��F@���@���@�dZ@���@���@��+@�v�@�=q@��@��-@�x�@�`B@�`B@�O�@�%@���@��u@�z�@�j@�Z@�A�@�9X@��@���@r:*@`N�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�S�A�VA�VA�S�A�VA�VA�XA�XA�ZA�S�A�O�A�G�A�C�A�9XA�33A�33A�1'A�+A��A���A�7LA��/A��A�XAأ�A՟�A���A�%AΏ\A�ZA̋DAʥ�A�ȴA�t�A�ƨA��AǅA��#Aď\A×�A�z�A���A��#A�t�A�
=A��PA��A���A�bA��A�hsA�^5A��PA�r�A��HA��A�+A�I�A�G�A�+A�bA�A�A��mA�9XA�1'A�\)A�|�A�l�A��A���A�-A�n�A�9XA�I�A��;A��hA�A�A�JA���A�JA��HA��A��A�bA�jA��HA��#A�O�A���A�`BA�  A�ZA��hA�33A�ƨA��A�\)A���A�oA��HA�VA�G�A�JA7LA};dA|�HA|ffAy��Aw��Ao�Am��Al9XAj�Ai�Ah��Af�yAe�FAdr�Ac��Ab��Aa�A_&�A]33A[;dAX�AV�HAR��AO��AN�yAN  ALr�AJ(�AGƨAD�AAƨA@�A>��A=/A;�A9�A81A5��A4�A2n�A0��A/�^A/�A.�!A.-A-�A+?}A*jA)O�A&M�A%"�A$�A$v�A"�yA��AO�A&�AVA��Az�A1AffA�hA��A�PA�RAXA��A��A�A�AȴA�9A�+A��AƨA�hAS�A
=AA�A��A��A
=qA	�hAĜA~�AZA5?A��A�AS�A (�@�S�@�O�@�dZ@�5?@��@�t�@���@���@�(�@�t�@�@���@땁@�V@�@�9X@�dZ@�n�@�O�@�1'@� �@�1@�@�t�@�S�@�@��@�Q�@���@ڏ\@�ff@�E�@��@ى7@�V@�Ĝ@�Z@��y@�n�@��T@�X@��/@�r�@�@ϥ�@���@͉7@́@̛�@�j@�Z@�Z@�Q�@�Q�@�I�@�9X@�b@�v�@��@�%@�p�@���@�C�@�;d@�+@�"�@��H@���@���@�Z@�t�@�E�@��@���@���@�%@��@�j@���@�K�@��w@�"�@�M�@�$�@�J@���@��@�
=@�33@�;d@���@�j@�z�@��u@�9X@��@�n�@�@�V@�^5@�=q@��@��@�9X@��
@��m@��w@�\)@�;d@�
=@��@�{@�?}@��@��@���@��`@��j@���@�r�@���@�ƨ@�ƨ@�ƨ@�t�@�@��@���@�ff@��@���@��h@�p�@��@��@�Z@��@��w@���@�t�@�33@�+@��@���@��@���@�p�@�7L@�Ĝ@��D@�Q�@�  @��P@�;d@��@��\@�5?@�@�hs@�G�@�&�@��@�V@��@�r�@� �@�ƨ@�K�@��\@�V@�ff@�^5@��@���@�J@�$�@�$�@�J@���@���@��-@��@���@�b@�b@��m@��
@��
@��@�t�@�+@�@���@�M�@��-@�p�@���@��9@�1@���@�^5@�x�@���@��j@�j@�A�@�b@��
@��F@��@���@�dZ@�33@��y@���@�~�@�M�@�5?@��@���@�?}@�&�@�Ĝ@�9X@� �@��@�1@��@�ƨ@���@��P@�|�@�t�@�K�@���@���@��+@�v�@�ff@�-@���@��T@��^@�p�@�/@�r�@�(�@���@�ƨ@��F@���@���@�dZ@���@���@��+@�v�@�=q@��@��-@�x�@�`B@�`B@�O�@�%@���@��u@�z�@�j@�Z@�A�@�9X@��@���@r:*@`N�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bk�Bk�Bk�Bk�Bk�Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bo�Bn�Bm�Bk�BhsBffBiyBiyBjBgmBbNB�+B��B�BDB�B#�B,B5?B;dBP�B`BBdZBu�B��B��B��B��B��B��B��B��B��B�{B�Bw�B~�Br�BhsBy�B�B�B�%B�+B�JB�PB�bB�VB�+B��B��B�=BB��B�XB�B�B.B[#B_;B�B	7B�B�B��B�5B��BƨB��Bp�B^5BR�BE�B33B-B�B\B	7BB
��B
�B
�
B
��B
��B
��B
�B
�1B
L�B
>wB
;dB
6FB
'�B
�B	�B	�HB	�B	��B	ŢB	��B	�LB	�B	��B	��B	��B	�hB	�B	v�B	jB	[#B	N�B	:^B	(�B	#�B	�B	�B	
=B��B�B�ZB�;B�
B��BɺBB�XB�B�B��B��B��B��B��B��B��B��B�oB�VB�=B�1B�+B�B�B�B�B�B�B�B}�B{�Bx�Bv�B{�B|�B}�B�B�B�B�+B�=B�DB�DB�JB�VB�VB�\B�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�7B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�?B�FB�LB�XB�XB�XB�RB�LB�LB�FB�B�3B�qB��B�;B�TB�ZB�ZB�ZB�ZB�`B�fB�fB	:^B	:^B	?}B	K�B	K�B	L�B	N�B	O�B	O�B	N�B	N�B	O�B	L�B	J�B	K�B	L�B	S�B	aHB	e`B	hsB	gmB	hsB	l�B	r�B	s�B	s�B	v�B	v�B	z�B	}�B	�%B	�7B	�=B	�=B	�=B	�DB	�DB	�PB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�?B	�FB	�FB	�LB	�XB	�jB	�qB	�}B	��B	ÖB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�`B	�ZB	�ZB	�TB	�TB	�ZB	�`B	�`B	�`B	�mB	�sB	�sB	�sB	�mB	�yB	�B	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
	7B
DB
DB
JB
JB
JB
JB
JB
PB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
{B
�B
'�B
7�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bk�Bk�Bk�Bk�Bk�Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bo�Bn�Bm�Bk�BhsBffBiyBiyBjBgmBbNB�+B��B�BDB�B#�B,B5?B;dBP�B`BBdZBu�B��B��B��B��B��B��B��B��B��B�{B�Bw�B~�Br�BhsBy�B�B�B�%B�+B�JB�PB�bB�VB�+B��B��B�=BB��B�XB�B�B.B[#B_;B�B	7B�B�B��B�5B��BƨB��Bp�B^5BR�BE�B33B-B�B\B	7BB
��B
�B
�
B
��B
��B
��B
�B
�1B
L�B
>wB
;dB
6FB
'�B
�B	�B	�HB	�B	��B	ŢB	��B	�LB	�B	��B	��B	��B	�hB	�B	v�B	jB	[#B	N�B	:^B	(�B	#�B	�B	�B	
=B��B�B�ZB�;B�
B��BɺBB�XB�B�B��B��B��B��B��B��B��B��B�oB�VB�=B�1B�+B�B�B�B�B�B�B�B}�B{�Bx�Bv�B{�B|�B}�B�B�B�B�+B�=B�DB�DB�JB�VB�VB�\B�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�7B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�?B�FB�LB�XB�XB�XB�RB�LB�LB�FB�B�3B�qB��B�;B�TB�ZB�ZB�ZB�ZB�`B�fB�fB	:^B	:^B	?}B	K�B	K�B	L�B	N�B	O�B	O�B	N�B	N�B	O�B	L�B	J�B	K�B	L�B	S�B	aHB	e`B	hsB	gmB	hsB	l�B	r�B	s�B	s�B	v�B	v�B	z�B	}�B	�%B	�7B	�=B	�=B	�=B	�DB	�DB	�PB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�?B	�FB	�FB	�LB	�XB	�jB	�qB	�}B	��B	ÖB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�`B	�ZB	�ZB	�TB	�TB	�ZB	�`B	�`B	�`B	�mB	�sB	�sB	�sB	�mB	�yB	�B	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
	7B
DB
DB
JB
JB
JB
JB
JB
PB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
{B
�B
'�B
7�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140822                              AO  ARCAADJP                                                                    20181024140822    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140822  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140822  QCF$                G�O�G�O�G�O�0               