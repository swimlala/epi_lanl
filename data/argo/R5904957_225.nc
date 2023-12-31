CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:47Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       B@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  a�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181024140847  20181024140847  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���&�T1   @���/h^2@5&�x���d��+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�33A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BO��BW��B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC1�fC4  C6  C8  C:  C<�C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  D   D �fD  D� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3�fD4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DS��DT� DU  DU�fDV  DV� DW  DW� DX  DXy�DY  DY� DZfDZ� D[fD[� D\  D\� D]  D]� D^  D^y�D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dry�Ds  Ds� Dt  Dt� Dt��Duy�Dv  Dv� Dv��Dw� Dw��Dyp D�%�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�\)A�\)A�(�A�(�A�(�B{B	{B{B{B!{B){B0�B9{BA{BI{BP�BX�Ba{Bi{Bq{By{B��=B��=B�W
B�=B�=B��=B�=B��=B��=C +�CECECECEC
ECEC^�CECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0+�C2+�C4EC6EC8EC:EC<^�C>EC@+�CBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�/\C�"�D HD ��DHD�HDHD�HDHD�HDHD�HDHD�HD
�D��D
�D�HDHD�HD	HD	�HD
HD
�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD
�D�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,�D,��D-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3�D3��D4HD4�HD5HD5�HD6HD6�HD7HD7�HD8
�D8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDK
�DK�HDLHDL�HDMHDM��DNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS��DT
�DT�HDUHDU��DVHDV�HDWHDW�HDXHDX��DYHDY�HDZ�DZ�HD[�D[�HD\HD\�HD]HD]�HD^HD^��D_
�D_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDf�Df�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDr
�Dr��DsHDs�HDtHDt�HDu
�Du��DvHDv�HDw
�Dw�HDw�Dy�HD�.g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�{A���A���A�x�A�-A��A�JA���A���A���A��A��mA��;Aé�AÅA�hsA�33A�A���A®AuAA�=qA�A��mA��#A��
A��TA��HA���A�ƨA�$�A���A�ƨA�=qA���A���A��A���A�oA��!A�bNA�ƨA�33A�bNA�z�A��PA�ƨA�A� �A�t�A�~�A���A���A�1'A�33A�ȴA���A�&�A��A���A��`A��/A�XA�ƨA���A�;dA�5?A�{A��
A���A�-A���A�G�A�x�A��9A�oA��9A��7A�9XA�z�A��^A���A��A}�A|9XA{K�AzM�Au�PAq��An�9An �Ak�Aj=qAh�Agl�Ad��Ad1'AcAa�-A_/A\  AZA�AU��AS\)ARAQl�AN��AM�^AL�AK�TAJ�`AJn�AI�7AG�AD{AA��AA�AA�A@bNA>��A=�A<z�A;A7��A5��A4��A3
=A0v�A/�A.�DA-;dA,ȴA+dZA*�uA*(�A(Q�A'\)A&ZA%��A%��A$�/A$1'A"�A!��A �`A��A�hA�A^5AA�A�A��AA�A��AM�A�#AhsA�9AM�AE�A�A��AVA �HA ��@�|�@���@��@�1'@��w@�\)@��@�
=@�E�@�`B@�1'@�v�@�X@�\)@��y@�`B@�Z@��@�~�@�o@�hs@���@�j@䛦@�@�@�Q�@��@�
=@�`B@߮@���@�=q@ݑh@�%@�A�@�t�@ڗ�@��@��@�/@��;@�n�@�O�@�1'@�l�@�n�@�@щ7@�Ĝ@�(�@���@̣�@�|�@�~�@�J@�@���@ɩ�@ɡ�@ə�@�G�@ț�@��@ǍP@���@�v�@�=q@���@�p�@ċD@��m@å�@��@�x�@��@���@� �@��;@�33@��R@�ff@���@���@�ƨ@��P@�;d@��y@��@��!@�5?@�bN@� �@���@�@� �@���@�ƨ@�@��!@��D@���@�33@�-@�{@�@���@�C�@�
=@��@��;@��j@�$�@��@��-@��h@�hs@�O�@���@���@�ȴ@�n�@�J@��^@��@���@�l�@���@��@�hs@��`@� �@�K�@�~�@�M�@��T@�hs@��@���@��@��`@��/@��/@��/@�Ĝ@��u@�A�@�1@���@�K�@�"�@��@�v�@�$�@��@���@��-@��h@�hs@�?}@�?}@�G�@��/@�(�@��;@�|�@�l�@�t�@�l�@���@�^5@��@�$�@�n�@���@�ff@���@��@���@���@��R@�=q@�O�@�O�@�7L@���@��@��@���@��9@��j@��j@�Ĝ@���@��j@�I�@�Z@�b@���@�A�@�A�@��F@��@�l�@�S�@�o@��y@���@���@�5?@�@���@�7L@��/@��D@��@��w@�C�@�o@�@���@���@�^5@�^5@�5?@�J@��7@�G�@�Ĝ@�r�@�1'@���@�S�@�@��+@�n�@�^5@�^5@�$�@���@�G�@���@�Ĝ@�j@�1'@���@��@�t�@�\)@��@�n�@�$�@��@��@���@�O�@�O�@�%@���@���@��j@�A�@���@��F@�l�@�"�@��@��H@�V@��@���@���@���@���@��7@�p�@�hs@�`B@�X@�q@xc�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A�{A���A���A�x�A�-A��A�JA���A���A���A��A��mA��;Aé�AÅA�hsA�33A�A���A®AuAA�=qA�A��mA��#A��
A��TA��HA���A�ƨA�$�A���A�ƨA�=qA���A���A��A���A�oA��!A�bNA�ƨA�33A�bNA�z�A��PA�ƨA�A� �A�t�A�~�A���A���A�1'A�33A�ȴA���A�&�A��A���A��`A��/A�XA�ƨA���A�;dA�5?A�{A��
A���A�-A���A�G�A�x�A��9A�oA��9A��7A�9XA�z�A��^A���A��A}�A|9XA{K�AzM�Au�PAq��An�9An �Ak�Aj=qAh�Agl�Ad��Ad1'AcAa�-A_/A\  AZA�AU��AS\)ARAQl�AN��AM�^AL�AK�TAJ�`AJn�AI�7AG�AD{AA��AA�AA�A@bNA>��A=�A<z�A;A7��A5��A4��A3
=A0v�A/�A.�DA-;dA,ȴA+dZA*�uA*(�A(Q�A'\)A&ZA%��A%��A$�/A$1'A"�A!��A �`A��A�hA�A^5AA�A�A��AA�A��AM�A�#AhsA�9AM�AE�A�A��AVA �HA ��@�|�@���@��@�1'@��w@�\)@��@�
=@�E�@�`B@�1'@�v�@�X@�\)@��y@�`B@�Z@��@�~�@�o@�hs@���@�j@䛦@�@�@�Q�@��@�
=@�`B@߮@���@�=q@ݑh@�%@�A�@�t�@ڗ�@��@��@�/@��;@�n�@�O�@�1'@�l�@�n�@�@щ7@�Ĝ@�(�@���@̣�@�|�@�~�@�J@�@���@ɩ�@ɡ�@ə�@�G�@ț�@��@ǍP@���@�v�@�=q@���@�p�@ċD@��m@å�@��@�x�@��@���@� �@��;@�33@��R@�ff@���@���@�ƨ@��P@�;d@��y@��@��!@�5?@�bN@� �@���@�@� �@���@�ƨ@�@��!@��D@���@�33@�-@�{@�@���@�C�@�
=@��@��;@��j@�$�@��@��-@��h@�hs@�O�@���@���@�ȴ@�n�@�J@��^@��@���@�l�@���@��@�hs@��`@� �@�K�@�~�@�M�@��T@�hs@��@���@��@��`@��/@��/@��/@�Ĝ@��u@�A�@�1@���@�K�@�"�@��@�v�@�$�@��@���@��-@��h@�hs@�?}@�?}@�G�@��/@�(�@��;@�|�@�l�@�t�@�l�@���@�^5@��@�$�@�n�@���@�ff@���@��@���@���@��R@�=q@�O�@�O�@�7L@���@��@��@���@��9@��j@��j@�Ĝ@���@��j@�I�@�Z@�b@���@�A�@�A�@��F@��@�l�@�S�@�o@��y@���@���@�5?@�@���@�7L@��/@��D@��@��w@�C�@�o@�@���@���@�^5@�^5@�5?@�J@��7@�G�@�Ĝ@�r�@�1'@���@�S�@�@��+@�n�@�^5@�^5@�$�@���@�G�@���@�Ĝ@�j@�1'@���@��@�t�@�\)@��@�n�@�$�@��@��@���@�O�@�O�@�%@���@���@��j@�A�@���@��F@�l�@�"�@��@��H@�V@��@���@���@���@���@��7@�p�@�hs@�`B@�X@�q@xc�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�PB�VB�hB��BŢB�B��BB+B1B	7BDBJBJBPB�B�B%�B49B@�BN�B[#BgmBq�B�B�=B�bB�hB�{B��B��B��B��B��B��B��B�{B�uB�uB�{B�bB�DB�B{�BffBZBQ�BD�B8RB2-B,B$�B�B{B\BB��B�HB��BĜB�FB�B��B�Bn�BaHBW
BH�B6FB(�B%�B �BhB
��B
�B
�5B
��B
ɺB
��B
�qB
�dB
�LB
�B
��B
��B
�1B
y�B
k�B
cTB
ZB
;dB
�B
%B	��B	�B	�NB	�)B	�B	ĜB	�wB	�XB	��B	��B	}�B	o�B	XB	I�B	D�B	@�B	33B	,B	&�B	�B	�B	�B	\B	B�sB�/B�;B�;B�5B�B�B�B��BƨB�jB�LB�B��B��B��B��B��B��B��B��B�hB�\B�PB�JB�DB�7B�%B�B�B� B}�B|�Bz�Bw�Bu�Bs�Br�Bq�Bq�Br�Bs�Bv�Bw�By�B{�B|�By�Bw�Bs�B_;B_;B_;B`BB_;Be`Be`BffBffBffBgmBiyBgmBjBl�Bn�Bm�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bp�Bp�Br�Bu�Bz�B~�B~�B�B�1B�\B�uB�{B��B��B��B�B�B�!B�'B�'B�-B�-B�-B�9B�?B�LB�RB�jB�wBBĜBĜBǮBɺB��B��B��B��B��B�B�B�#B�)B�)B�TB�ZB�`B�sB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B�B�B�yB�fB�`B�TB�TB�yB�yB�B�B��B	B		7B	oB	�B	�B	)�B	33B	5?B	6FB	7LB	7LB	:^B	>wB	B�B	D�B	F�B	I�B	M�B	N�B	Q�B	S�B	VB	W
B	YB	\)B	`BB	dZB	ffB	jB	m�B	o�B	p�B	p�B	q�B	q�B	q�B	q�B	q�B	r�B	u�B	w�B	z�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�7B	�7B	�JB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�3B	�9B	�LB	�LB	�XB	�dB	�qB	��B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�5B	�BB	�HB	�HB	�HB	�NB	�NB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
�B
!|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�PB�VB�hB��BŢB�B��BB+B1B	7BDBJBJBPB�B�B%�B49B@�BN�B[#BgmBq�B�B�=B�bB�hB�{B��B��B��B��B��B��B��B�{B�uB�uB�{B�bB�DB�B{�BffBZBQ�BD�B8RB2-B,B$�B�B{B\BB��B�HB��BĜB�FB�B��B�Bn�BaHBW
BH�B6FB(�B%�B �BhB
��B
�B
�5B
��B
ɺB
��B
�qB
�dB
�LB
�B
��B
��B
�1B
y�B
k�B
cTB
ZB
;dB
�B
%B	��B	�B	�NB	�)B	�B	ĜB	�wB	�XB	��B	��B	}�B	o�B	XB	I�B	D�B	@�B	33B	,B	&�B	�B	�B	�B	\B	B�sB�/B�;B�;B�5B�B�B�B��BƨB�jB�LB�B��B��B��B��B��B��B��B��B�hB�\B�PB�JB�DB�7B�%B�B�B� B}�B|�Bz�Bw�Bu�Bs�Br�Bq�Bq�Br�Bs�Bv�Bw�By�B{�B|�By�Bw�Bs�B_;B_;B_;B`BB_;Be`Be`BffBffBffBgmBiyBgmBjBl�Bn�Bm�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bp�Bp�Br�Bu�Bz�B~�B~�B�B�1B�\B�uB�{B��B��B��B�B�B�!B�'B�'B�-B�-B�-B�9B�?B�LB�RB�jB�wBBĜBĜBǮBɺB��B��B��B��B��B�B�B�#B�)B�)B�TB�ZB�`B�sB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B�B�B�yB�fB�`B�TB�TB�yB�yB�B�B��B	B		7B	oB	�B	�B	)�B	33B	5?B	6FB	7LB	7LB	:^B	>wB	B�B	D�B	F�B	I�B	M�B	N�B	Q�B	S�B	VB	W
B	YB	\)B	`BB	dZB	ffB	jB	m�B	o�B	p�B	p�B	q�B	q�B	q�B	q�B	q�B	r�B	u�B	w�B	z�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�7B	�7B	�JB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�3B	�9B	�LB	�LB	�XB	�dB	�qB	��B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�5B	�BB	�HB	�HB	�HB	�NB	�NB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
�B
!|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140847                              AO  ARCAADJP                                                                    20181024140847    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140847  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140847  QCF$                G�O�G�O�G�O�0               