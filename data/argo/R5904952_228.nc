CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:57Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190557  20181005190557  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�eW�1   @��j���@0�l�C���c��^5?}1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A���A���A�  A�  A�  B   B��B��B��B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C��3C�  C��C�  C��D fD � D  D�fD  D� D  D�fD  D� D  D� D  D� D��Dy�D��D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D��Dy�D��Dy�D  D�fDfD� D  D� DfDy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%�fD&  D&� D&��D'� D(fD(�fD)fD)�fD*  D*y�D+  D+�fD,fD,�fD-  D-y�D.  D.� D/  D/� D0fD0� D1  D1� D2  D2� D2��D3y�D4  D4�fD5fD5� D5��D6y�D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;�fD<  D<y�D<��D=� D>fD>� D>��D?� D@  D@�fDA  DAy�DA��DB� DC  DC� DDfDD� DD��DE� DF  DF� DG  DG� DH  DH� DH��DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DN��DOy�DO��DP�fDQfDQ�fDRfDR�fDSfDS� DT  DT� DUfDU�fDVfDV�fDW  DW� DX  DXy�DX��DY� DZ  DZ� D[  D[y�D[��D\� D]fD]� D]��D^� D_  D_� D`fD`� Da  Da� Db  Db� Db��Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg�fDhfDh�fDi  Di� DjfDj� Dj��Dky�Dl  Dl� Dl��Dmy�Dn  Dn� DofDo�fDo��Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Ds��Dt� Du  Du�fDv  Dv� Dw  Dw� DxfDxL�Dy��D�8�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�=q@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A���A���A�(�A�(�A�(�B{B�B�B�B!{B){B1{B9{BA{BI{BQz�BYz�Ba{Bi{Bqz�By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B�W
B��=B��=B��=B��=B��=BĊ=BȊ=B̽pBЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��=B��=C ECEC^�C^�C^�C
ECECECECECECECECECECEC EC"+�C$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<^�C>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`+�CbECd^�CfEChECjEClECnECpECr+�CtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�/\C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C��C�"�C�/\C�/\C�/\C�/\C�"�C��C�"�C�/\C�"�C�/\D �D �HDHD��DHD�HDHD��DHD�HDHD�HDHD�HD
�D��D
�D�HD	HD	�HD
HD
�HD
�D�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD��D
�D��D
�D��DHD��D�D�HDHD�HD�D��D
�D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$��D%HD%��D&HD&�HD'
�D'�HD(�D(��D)�D)��D*HD*��D+HD+��D,�D,��D-HD-��D.HD.�HD/HD/�HD0�D0�HD1HD1�HD2HD2�HD3
�D3��D4HD4��D5�D5�HD6
�D6��D7HD7�HD8HD8�HD9HD9��D:HD:�HD;HD;��D<HD<��D=
�D=�HD>�D>�HD?
�D?�HD@HD@��DAHDA��DB
�DB�HDCHDC�HDD�DD�HDE
�DE�HDFHDF�HDGHDG�HDHHDH�HDI
�DI��DJHDJ�HDKHDK�HDLHDL�HDMHDM�HDN
�DN�HDO
�DO��DP
�DP��DQ�DQ��DR�DR��DS�DS�HDTHDT�HDU�DU��DV�DV��DWHDW�HDXHDX��DY
�DY�HDZHDZ�HD[HD[��D\
�D\�HD]�D]�HD^
�D^�HD_HD_�HD`�D`�HDaHDa�HDbHDb�HDc
�Dc�HDdHDd��DeHDe�HDfHDf�HDgHDg��Dh�Dh��DiHDi�HDj�Dj�HDk
�Dk��DlHDl�HDm
�Dm��DnHDn�HDo�Do��Dp
�Dp�HDqHDq�HDrHDr�HDsHDs��Dt
�Dt�HDuHDu��DvHDv�HDwHDw�HDx�Dx^Dy��D�AHD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��`A��TA��HA��TA��mA��A��A��A���A�  A�  A���A�oA�&�A�33A��`Aɉ7A�1A�1A���AǮA��mA�$�A�?}A��mAǲ-A���A�Q�A�"�A�bA���A��#A���Aƴ9A�~�A��AŰ!A�p�A��/A�M�A�t�A�S�A�ZA��A��A�(�A�A�A�n�A�hsA�Q�A��A��uA�E�A�;dA�bA���A�A��A���A� �A�
=A�r�A�/A��A�%A��wA��A���A�9XA�jA�t�A�7LA��;A�oA��PA���A��HA���A�A�A�K�A�1A�^5A���A�(�A���A�1A��yA���A���A�I�A���A��wA��\A�1A��yA��jA��9A��!A�ȴA��A���A��/A��PA}\)Ax��Au7LAr�uAp��Ak�;Ag��Ad1'Abv�A]�wA[O�AY�AXv�AV�jATbNARv�AO�AMG�AJ��AI�^AH�/AH1'AGƨAG?}AC�wAA��AAx�A?��A=�A<5?A;K�A9�A6�jA5��A4�jA3|�A1"�A.��A.1'A-|�A+?}A*A�A)��A(1'A'dZA'A&ZA%?}A$�A$5?A#+A"�A"��A"E�A"$�A!�A!�-A!�hA!S�A �yA -A{A9XA��A-AK�A��A%Av�A~�A��AS�A��A�A��A5?A��A�A�wAdZA
�HA
E�Az�A�yA-A�A��A
=A��A~�A�wA I�@�
=@�\)@�dZ@�5?@��@��
@�@�@���@�1'@�~�@�7@�p�@���@�ƨ@�!@��#@�1'@�ƨ@�  @���@���@�33@�{@�?}@��@��y@ݡ�@�&�@ܬ@��m@�t�@�`B@׶F@�K�@�@�X@�/@ӕ�@��#@�G�@�`B@ёh@щ7@�/@�%@Ѓ@�(�@�dZ@ΰ!@���@���@�b@˝�@�ff@ɩ�@��@�A�@Ǖ�@�C�@�ȴ@�v�@�5?@őh@�bN@�1'@��@���@�dZ@�ȴ@�$�@��-@��h@�hs@�r�@��@��!@�~�@��@�E�@�@��-@�`B@��@���@��j@�A�@��@���@���@��#@�&�@��D@�j@�bN@�bN@�Z@�Q�@�1@�dZ@��y@��+@�M�@�-@�$�@�=q@�J@��@�x�@���@�x�@�7L@�Ĝ@���@�l�@�33@�o@���@�^5@�=q@�=q@�=q@�5?@��@��T@��#@�J@��@��@��`@�b@���@�\)@�S�@�\)@���@�33@�ff@���@�7L@���@��u@�Z@�A�@�A�@�I�@�j@�9X@��@�K�@��+@�-@���@�@�p�@�/@�hs@���@��D@��@�dZ@���@�I�@��;@��@��@�v�@�%@���@���@�j@���@���@���@��H@��@�o@��@���@��H@���@�E�@���@���@��@�x�@�X@��@�j@���@��@�
=@�v�@�E�@���@�@���@�O�@��`@��D@�r�@�I�@�9X@�1'@�1'@� �@�1@��m@�ƨ@��@�S�@�;d@��@���@��@��-@�@��^@���@�G�@�?}@�O�@�&�@���@�b@�S�@�+@��@���@��+@�v�@�ff@�5?@��@��@��@��#@���@��h@�%@���@�(�@��m@��F@�S�@�;d@�C�@�
=@��+@�-@��T@���@��h@�X@��/@�z�@�1'@���@��F@��@�K�@�@��R@��+@�^5@�$�@�@�p�@�?}@�O�@�hs@�p�@�X@�?}@��@��`@��j@��@�bN@�(�@��F@�t�@�33@���@�ȴ@�ff@�@��#@���@�?}@��`@��9@��D@�9X@��m@��@��@�\)@�"�@��H@���@�9X@x�@f҉1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��`A��TA��HA��TA��mA��A��A��A���A�  A�  A���A�oA�&�A�33A��`Aɉ7A�1A�1A���AǮA��mA�$�A�?}A��mAǲ-A���A�Q�A�"�A�bA���A��#A���Aƴ9A�~�A��AŰ!A�p�A��/A�M�A�t�A�S�A�ZA��A��A�(�A�A�A�n�A�hsA�Q�A��A��uA�E�A�;dA�bA���A�A��A���A� �A�
=A�r�A�/A��A�%A��wA��A���A�9XA�jA�t�A�7LA��;A�oA��PA���A��HA���A�A�A�K�A�1A�^5A���A�(�A���A�1A��yA���A���A�I�A���A��wA��\A�1A��yA��jA��9A��!A�ȴA��A���A��/A��PA}\)Ax��Au7LAr�uAp��Ak�;Ag��Ad1'Abv�A]�wA[O�AY�AXv�AV�jATbNARv�AO�AMG�AJ��AI�^AH�/AH1'AGƨAG?}AC�wAA��AAx�A?��A=�A<5?A;K�A9�A6�jA5��A4�jA3|�A1"�A.��A.1'A-|�A+?}A*A�A)��A(1'A'dZA'A&ZA%?}A$�A$5?A#+A"�A"��A"E�A"$�A!�A!�-A!�hA!S�A �yA -A{A9XA��A-AK�A��A%Av�A~�A��AS�A��A�A��A5?A��A�A�wAdZA
�HA
E�Az�A�yA-A�A��A
=A��A~�A�wA I�@�
=@�\)@�dZ@�5?@��@��
@�@�@���@�1'@�~�@�7@�p�@���@�ƨ@�!@��#@�1'@�ƨ@�  @���@���@�33@�{@�?}@��@��y@ݡ�@�&�@ܬ@��m@�t�@�`B@׶F@�K�@�@�X@�/@ӕ�@��#@�G�@�`B@ёh@щ7@�/@�%@Ѓ@�(�@�dZ@ΰ!@���@���@�b@˝�@�ff@ɩ�@��@�A�@Ǖ�@�C�@�ȴ@�v�@�5?@őh@�bN@�1'@��@���@�dZ@�ȴ@�$�@��-@��h@�hs@�r�@��@��!@�~�@��@�E�@�@��-@�`B@��@���@��j@�A�@��@���@���@��#@�&�@��D@�j@�bN@�bN@�Z@�Q�@�1@�dZ@��y@��+@�M�@�-@�$�@�=q@�J@��@�x�@���@�x�@�7L@�Ĝ@���@�l�@�33@�o@���@�^5@�=q@�=q@�=q@�5?@��@��T@��#@�J@��@��@��`@�b@���@�\)@�S�@�\)@���@�33@�ff@���@�7L@���@��u@�Z@�A�@�A�@�I�@�j@�9X@��@�K�@��+@�-@���@�@�p�@�/@�hs@���@��D@��@�dZ@���@�I�@��;@��@��@�v�@�%@���@���@�j@���@���@���@��H@��@�o@��@���@��H@���@�E�@���@���@��@�x�@�X@��@�j@���@��@�
=@�v�@�E�@���@�@���@�O�@��`@��D@�r�@�I�@�9X@�1'@�1'@� �@�1@��m@�ƨ@��@�S�@�;d@��@���@��@��-@�@��^@���@�G�@�?}@�O�@�&�@���@�b@�S�@�+@��@���@��+@�v�@�ff@�5?@��@��@��@��#@���@��h@�%@���@�(�@��m@��F@�S�@�;d@�C�@�
=@��+@�-@��T@���@��h@�X@��/@�z�@�1'@���@��F@��@�K�@�@��R@��+@�^5@�$�@�@�p�@�?}@�O�@�hs@�p�@�X@�?}@��@��`@��j@��@�bN@�(�@��F@�t�@�33@���@�ȴ@�ff@�@��#@���@�?}@��`@��9@��D@�9X@��m@��@��@�\)@�"�@��H@���@�9X@x�@f҉1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�TB	�NB	�NB	�NB	�ZB	�sB	�B	�B	��B	��B	��B	��B
B
JB
{B
�B	��B	ŢB	��B	��B	�)B	��B
hB
$�B
"�B
?}B
z�B
w�B
�B
�1B
��B
�3B
��B
�B
�B �B6FBN�B�B�uB��B�wB��B�B�5B�yB�BB\BoB�B �B6FBB�BC�BG�BE�B@�B;dB<jBF�BD�BB�BA�B?}B<jB6FB0!B,B(�B%�B�B�BPB%B��B�B�yB�
B�B��B�VB�+Bz�BdZBM�B>wB.B!�B�B
��B
��B
��B
B
��B
�%B
iyB
XB
/B
�B
bB
B	�`B	��B	�B	��B	�B	t�B	VB	=qB	)�B	�B	1B��B�B�mB�BB�B��BÖB�XB�B��B��B��B��B��B��B��B��B�oB�7B�B� B}�B� B�B�B�B�B�+B�=B�=B�bB��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�'B�-B�3B�3B�9B�LB�dB�dB�^B�RB�dBƨB��B��BƨBĜB��B��B��B��B��B��B��B��B�HB�B��B�B�B�TB�;B�/B�NB�ZB�HB�/B�B��BɺBĜBɺBƨB�qB�RB�qBĜBŢBƨBǮBɺB��B��B��B��B�B�)B�BB�ZB�`B�ZB�`B�B�yB�sB�B�B�B�B��B��B��B��B	B	B	1B	DB	
=B	1B	PB	{B	�B	�B	�B	�B	"�B	#�B	#�B	#�B	%�B	'�B	(�B	)�B	,B	/B	1'B	49B	7LB	8RB	;dB	=qB	?}B	C�B	H�B	K�B	M�B	P�B	R�B	VB	XB	YB	XB	W
B	S�B	Q�B	T�B	XB	ffB	k�B	m�B	o�B	p�B	q�B	r�B	r�B	s�B	r�B	u�B	{�B	{�B	|�B	� B	� B	�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�DB	�JB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�FB	�9B	�-B	�!B	�!B	�-B	�3B	�3B	�^B	�jB	�jB	�jB	�jB	�qB	�qB	�wB	�}B	��B	ÖB	ŢB	ƨB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	�B	��B	�
B	�B	�;B	�`B	�mB	�B	�B	�B	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B	��B	��B
B
B
+B
+B
1B
+B
+B
+B
+B
+B
+B
1B
1B
1B
+B
1B

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
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
6zB
?H2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B	�TB	�NB	�NB	�NB	�ZB	�sB	�B	�B	��B	��B	��B	��B
B
JB
{B
�B	��B	ŢB	��B	��B	�)B	��B
hB
$�B
"�B
?}B
z�B
w�B
�B
�1B
��B
�3B
��B
�B
�B �B6FBN�B�B�uB��B�wB��B�B�5B�yB�BB\BoB�B �B6FBB�BC�BG�BE�B@�B;dB<jBF�BD�BB�BA�B?}B<jB6FB0!B,B(�B%�B�B�BPB%B��B�B�yB�
B�B��B�VB�+Bz�BdZBM�B>wB.B!�B�B
��B
��B
��B
B
��B
�%B
iyB
XB
/B
�B
bB
B	�`B	��B	�B	��B	�B	t�B	VB	=qB	)�B	�B	1B��B�B�mB�BB�B��BÖB�XB�B��B��B��B��B��B��B��B��B�oB�7B�B� B}�B� B�B�B�B�B�+B�=B�=B�bB��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�'B�-B�3B�3B�9B�LB�dB�dB�^B�RB�dBƨB��B��BƨBĜB��B��B��B��B��B��B��B��B�HB�B��B�B�B�TB�;B�/B�NB�ZB�HB�/B�B��BɺBĜBɺBƨB�qB�RB�qBĜBŢBƨBǮBɺB��B��B��B��B�B�)B�BB�ZB�`B�ZB�`B�B�yB�sB�B�B�B�B��B��B��B��B	B	B	1B	DB	
=B	1B	PB	{B	�B	�B	�B	�B	"�B	#�B	#�B	#�B	%�B	'�B	(�B	)�B	,B	/B	1'B	49B	7LB	8RB	;dB	=qB	?}B	C�B	H�B	K�B	M�B	P�B	R�B	VB	XB	YB	XB	W
B	S�B	Q�B	T�B	XB	ffB	k�B	m�B	o�B	p�B	q�B	r�B	r�B	s�B	r�B	u�B	{�B	{�B	|�B	� B	� B	�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�DB	�JB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�FB	�9B	�-B	�!B	�!B	�-B	�3B	�3B	�^B	�jB	�jB	�jB	�jB	�qB	�qB	�wB	�}B	��B	ÖB	ŢB	ƨB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	�B	��B	�
B	�B	�;B	�`B	�mB	�B	�B	�B	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B	��B	��B
B
B
+B
+B
1B
+B
+B
+B
+B
+B
+B
1B
1B
1B
+B
1B

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
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
6zB
?H2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190557                              AO  ARCAADJP                                                                    20181005190557    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190557  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190557  QCF$                G�O�G�O�G�O�8000            