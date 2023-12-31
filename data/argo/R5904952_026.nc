CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:11Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190511  20181005190511  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׳��I��1   @׳�33E�@2�$�/��c���"��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @�33@���A   A   A@  A`  A~ffA�  A���A���A���A���A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BO��BW��B`  BhffBp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��D   D � DfD� D��Dy�D  D� D  D� D  Dy�D��D� DfD� D  Dy�D��D	y�D
  D
� D  D� DfD� D  D� D  D� D  Dy�D  D� D  Dy�D��D� DfD� D  D� D��Dy�D��Dy�D  D�fDfD�fDfD� D��D� D  D� D��D� DfD�fD  D� D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3�fD4fD4�fD5  D5� D6fD6�fD7  D7� D8fD8� D9  D9� D:  D:� D;  D;y�D;��D<y�D=  D=� D>  D>� D?  D?� D@  D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DE  DE� DFfDF� DG  DG� DH  DHy�DI  DI� DI��DJ� DJ��DK� DL  DL� DL��DM� DN  DN� DOfDO� DP  DP�fDQ  DQ� DQ��DR� DSfDS�fDT  DTy�DT��DU� DU��DVy�DW  DW�fDX  DX�fDY  DYy�DZ  DZ� D[fD[�fD\fD\�fD]fD]�fD^fD^� D^��D_y�D_��D`� DafDa�fDbfDb�fDb��Dc� DdfDd�fDefDe�fDf  Dfy�Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� DtfDt�fDufDu� Dv  Dv�fDw  Dw� Dw�fDy� D�%�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@�Q�AA!AAAaA�{A��HA��A��A��AѮA��HA��HB p�B�
Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BP
>BX
>B`p�Bh�
Bpp�Bxp�B�8RB�8RB�8RB�8RB�B�8RB�8RB�8RB�8RB�8RB�B�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*5�C,)C.)C0)C2)C4)C65�C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV5�CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�GC�C�C�C�C��C�C�C�C�C�C�C�C�C�C��C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�GC�C�C�GC�C�C�C�C�C�C�C�C�C�C�C�C�GC�GC�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�GC�C�C�C�C�C�GC�C�C�C�C�C�C�C��D 
D �
DpD�
D �D��D
D�
D
D�
D
D��D �D�
DpD�
D
D��D	 �D	��D

D
�
D
D�
DpD�
D
D�
D
D�
D
D��D
D�
D
D��D �D�
DpD�
D
D�
D �D��D �D��D
D�pDpD�pDpD�
D �D�
D
D�
D �D�
DpD�pD
D�
D*�
D+
D+�
D,pD,�
D-
D-�
D.
D.�
D/
D/�
D0
D0�
D1
D1�
D2
D2�
D3pD3�pD4pD4�pD5
D5�
D6pD6�pD7
D7�
D8pD8�
D9
D9�
D:
D:�
D;
D;��D< �D<��D=
D=�
D>
D>�
D?
D?�
D@
D@��DA �DA��DB �DB��DC �DC��DD �DD��DE
DE�
DFpDF�
DG
DG�
DH
DH��DI
DI�
DJ �DJ�
DK �DK�
DL
DL�
DM �DM�
DN
DN�
DOpDO�
DP
DP�pDQ
DQ�
DR �DR�
DSpDS�pDT
DT��DU �DU�
DV �DV��DW
DW�pDX
DX�pDY
DY��DZ
DZ�
D[pD[�pD\pD\�pD]pD]�pD^pD^�
D_ �D_��D` �D`�
DapDa�pDbpDb�pDc �Dc�
DdpDd�pDepDe�pDf
Df��Dg
Dg�
Dh
Dh�
Di
Di�
DjpDj�
Dk
Dk�
Dl
Dl�
Dm
Dm�
Dn
Dn�
Do
Do��Dp
Dp�
Dq
Dq�
Dr
Dr�
Ds �Ds�
DtpDt�pDupDu�
Dv
Dv�pDw
Dw�
Dw�pDy�
D�)HD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A���A���A���A���A���A���A�A�A�A�A�A�%A�A�A�
=A�
=A�VAՙ�A��mA�5?A�%A�ĜAӋDA�S�A�oA�ƨAң�AґhA� �A��A�v�A���A�I�A��A�A�A�A�?}A�t�Aʡ�A�Aɩ�A�|�A�Q�AȶFA���A�33AƮA� �A���A��A�1A��TAÑhA§�A�1'A��`A���A��RA�$�A��!A�JA�l�A�oA�7LA��7A��uA�1A��^A��A��A�z�A��FA�%A�jA�E�A�M�A�XA�33A���A��wA���A��\A�?}A��jA�7LA�l�A�p�A��A��wA�A�n�A���A�(�A���A���A��A���A��
A�bA��`A���A�$�A��7A�1A���A��A��FA�ȴA�l�A~-A|ȴA{/Ay��Ay�PAu�hAqK�Ap��Ap�jAp^5An�jAm��Am�Al5?Ag&�Ac%A[�^AX��AWl�AUƨAS33AP��AOoAM�mALz�AJ�HAG�7AFE�AD^5ABA�AAG�A?��A=�PA;�A9
=A8v�A89XA8  A7C�A6-A2r�A1XA0�\A/�mA-��A*�+A(��A'�A&�A%t�A"��A!dZA�A�A/A�;A/A�RA�A%AĜA��A�DA�Ax�A"�A��A�DA�A33A�A�DA�;A�-A�A�DA �A�A��A��A�AVA5?A�A�AC�A	�A��A�DAv�A9XA{A�-AdZA&�A
=A��AJA33A?}A�jA�\AA�#A�hA"�A�\A��A ��@��F@��+@��9@� �@�"�@�~�@�V@���@��^@�9X@���@�@��@��@�7@���@��@�=q@�G�@�1@��H@���@�C�@�\@�O�@߾w@ݑh@�&�@���@�bN@� �@���@��@�@�$�@��@�"�@���@ёh@�X@�r�@Ϯ@ϕ�@�S�@��y@���@Ο�@�J@���@�@ͩ�@�Ĝ@˶F@���@�bN@� �@�+@őh@��@ēu@�j@�t�@�l�@���@���@�%@��j@�Z@�1@�S�@�o@�K�@�\)@�l�@�t�@�;d@�@���@��+@�M�@���@��^@�O�@�Ĝ@��@�Q�@� �@���@�+@�ff@�p�@���@�dZ@�33@�33@�l�@�Q�@�1'@��F@��@�@��H@���@�@��@�hs@�?}@�/@��j@�b@��
@�@���@��+@�V@���@�G�@��/@��m@���@�t�@�|�@�t�@�l�@��!@�v�@�ff@�J@�@��@���@��R@�~�@�{@��@���@�1'@��@�(�@�bN@�Ĝ@��@��D@��@�ƨ@�33@��y@���@�=q@��@���@���@��@���@��7@�hs@��@��@��@�\)@���@��+@�v�@�ff@���@���@���@��
@�1@�A�@��m@�C�@�ȴ@�v�@�~�@�^5@�E�@�E�@��@���@�5?@��+@��y@��^@��@���@�1'@�"�@��@���@�E�@���@���@��7@�O�@�V@���@���@��@���@��@��/@���@��9@���@��u@�I�@���@��P@���@���@�|�@�\)@�dZ@�dZ@�33@�@��y@�ȴ@���@�n�@�V@��@���@��^@�G�@���@��@�A�@��
@�dZ@�
=@��@��R@���@�V@�{@���@�&�@���@��/@���@�Q�@��@��;@��F@�\)@�"�@�+@��@�M�@��#@��@�C�@p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A���A���A���A���A���A���A�A�A�A�A�A�%A�A�A�
=A�
=A�VAՙ�A��mA�5?A�%A�ĜAӋDA�S�A�oA�ƨAң�AґhA� �A��A�v�A���A�I�A��A�A�A�A�?}A�t�Aʡ�A�Aɩ�A�|�A�Q�AȶFA���A�33AƮA� �A���A��A�1A��TAÑhA§�A�1'A��`A���A��RA�$�A��!A�JA�l�A�oA�7LA��7A��uA�1A��^A��A��A�z�A��FA�%A�jA�E�A�M�A�XA�33A���A��wA���A��\A�?}A��jA�7LA�l�A�p�A��A��wA�A�n�A���A�(�A���A���A��A���A��
A�bA��`A���A�$�A��7A�1A���A��A��FA�ȴA�l�A~-A|ȴA{/Ay��Ay�PAu�hAqK�Ap��Ap�jAp^5An�jAm��Am�Al5?Ag&�Ac%A[�^AX��AWl�AUƨAS33AP��AOoAM�mALz�AJ�HAG�7AFE�AD^5ABA�AAG�A?��A=�PA;�A9
=A8v�A89XA8  A7C�A6-A2r�A1XA0�\A/�mA-��A*�+A(��A'�A&�A%t�A"��A!dZA�A�A/A�;A/A�RA�A%AĜA��A�DA�Ax�A"�A��A�DA�A33A�A�DA�;A�-A�A�DA �A�A��A��A�AVA5?A�A�AC�A	�A��A�DAv�A9XA{A�-AdZA&�A
=A��AJA33A?}A�jA�\AA�#A�hA"�A�\A��A ��@��F@��+@��9@� �@�"�@�~�@�V@���@��^@�9X@���@�@��@��@�7@���@��@�=q@�G�@�1@��H@���@�C�@�\@�O�@߾w@ݑh@�&�@���@�bN@� �@���@��@�@�$�@��@�"�@���@ёh@�X@�r�@Ϯ@ϕ�@�S�@��y@���@Ο�@�J@���@�@ͩ�@�Ĝ@˶F@���@�bN@� �@�+@őh@��@ēu@�j@�t�@�l�@���@���@�%@��j@�Z@�1@�S�@�o@�K�@�\)@�l�@�t�@�;d@�@���@��+@�M�@���@��^@�O�@�Ĝ@��@�Q�@� �@���@�+@�ff@�p�@���@�dZ@�33@�33@�l�@�Q�@�1'@��F@��@�@��H@���@�@��@�hs@�?}@�/@��j@�b@��
@�@���@��+@�V@���@�G�@��/@��m@���@�t�@�|�@�t�@�l�@��!@�v�@�ff@�J@�@��@���@��R@�~�@�{@��@���@�1'@��@�(�@�bN@�Ĝ@��@��D@��@�ƨ@�33@��y@���@�=q@��@���@���@��@���@��7@�hs@��@��@��@�\)@���@��+@�v�@�ff@���@���@���@��
@�1@�A�@��m@�C�@�ȴ@�v�@�~�@�^5@�E�@�E�@��@���@�5?@��+@��y@��^@��@���@�1'@�"�@��@���@�E�@���@���@��7@�O�@�V@���@���@��@���@��@��/@���@��9@���@��u@�I�@���@��P@���@���@�|�@�\)@�dZ@�dZ@�33@�@��y@�ȴ@���@�n�@�V@��@���@��^@�G�@���@��@�A�@��
@�dZ@�
=@��@��R@���@�V@�{@���@�&�@���@��/@���@�Q�@��@��;@��F@�\)@�"�@�+@��@�M�@��#@��@�C�@p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
q�B
p�B
�\B
�}B  B �B'�B.B49B;dBA�BC�BD�BE�BE�B@�B9XB6FB)�B(�B�B�B�B=qBT�Bl�B�B�{B��B�FB��BƨBƨBĜBǮB��B��B�B�TB�yB�B��B�B�B#�B)�B,B<jBJ�BS�BM�BR�BP�BG�BM�BQ�BT�BYBhsBp�Bq�Bw�By�Br�Be`BJ�BA�B:^B.B#�B�B
=B�B�wB�DBB�BDBB
��B
��B
�ZB
�!B
�B
x�B
YB
E�B
?}B
$�B
�B
�B
�B
{B	��B	�B	�#B	��B	ƨB	�jB	�RB	��B	�hB	�{B	�{B	�uB	�DB	�B	�B	w�B	t�B	`BB	@�B	.B	&�B	�B	bB	B��B��B�B�fB�#B��B��B��BƨBB�qB�jB�jB�jB�dB�^B�^B�qBB�}B�qB�^B�3B�B��B��B��B�-B�-B�-B�B�B�B�B�B�B�B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B�dB�jB�}B��B��B�}B�jB�dBŢB��B��B��B��B��B��B��BɺBƨB��B��B��B��B��B��B��B�B�5B�BB�BB�BB�HB�;B��B��BǮBĜBB�wB�^B�LB�LB�jB�jB�XB�dBBƨBǮBƨBƨBȴBĜBĜBƨB��B��B��B��B��B��B�B�
B�#B�/B�/B�NB�B�B��B��B��B��B��B��B��B��B��B	B	JB	�B	�B	$�B	(�B	)�B	(�B	(�B	'�B	(�B	)�B	,B	,B	-B	.B	2-B	5?B	8RB	9XB	9XB	9XB	9XB	:^B	:^B	;dB	=qB	=qB	=qB	=qB	:^B	7LB	5?B	:^B	>wB	A�B�DB	v�B	x�B	y�B	y�B	{�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�3B	�?B	�RB	�wB	B	ĜB	ǮB	ƨB	ŢB	ĜB	ÖB	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�#B	�;B	�;B	�5B	�;B	�;B	�BB	�HB	�BB	�/B	�#B	�5B	�HB	�HB	�;B	�5B	�/B	�5B	�;B	�BB	�BB	�)B	�
B	��B	�B	�B	�#B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�NB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
DB
�B
eB
$�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
q�B
p�B
�\B
�}B  B �B'�B.B49B;dBA�BC�BD�BE�BE�B@�B9XB6FB)�B(�B�B�B�B=qBT�Bl�B�B�{B��B�FB��BƨBƨBĜBǮB��B��B�B�TB�yB�B��B�B�B#�B)�B,B<jBJ�BS�BM�BR�BP�BG�BM�BQ�BT�BYBhsBp�Bq�Bw�By�Br�Be`BJ�BA�B:^B.B#�B�B
=B�B�wB�DBB�BDBB
��B
��B
�ZB
�!B
�B
x�B
YB
E�B
?}B
$�B
�B
�B
�B
{B	��B	�B	�#B	��B	ƨB	�jB	�RB	��B	�hB	�{B	�{B	�uB	�DB	�B	�B	w�B	t�B	`BB	@�B	.B	&�B	�B	bB	B��B��B�B�fB�#B��B��B��BƨBB�qB�jB�jB�jB�dB�^B�^B�qBB�}B�qB�^B�3B�B��B��B��B�-B�-B�-B�B�B�B�B�B�B�B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B�dB�jB�}B��B��B�}B�jB�dBŢB��B��B��B��B��B��B��BɺBƨB��B��B��B��B��B��B��B�B�5B�BB�BB�BB�HB�;B��B��BǮBĜBB�wB�^B�LB�LB�jB�jB�XB�dBBƨBǮBƨBƨBȴBĜBĜBƨB��B��B��B��B��B��B�B�
B�#B�/B�/B�NB�B�B��B��B��B��B��B��B��B��B��B	B	JB	�B	�B	$�B	(�B	)�B	(�B	(�B	'�B	(�B	)�B	,B	,B	-B	.B	2-B	5?B	8RB	9XB	9XB	9XB	9XB	:^B	:^B	;dB	=qB	=qB	=qB	=qB	:^B	7LB	5?B	:^B	>wB	A�B�DB	v�B	x�B	y�B	y�B	{�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�3B	�?B	�RB	�wB	B	ĜB	ǮB	ƨB	ŢB	ĜB	ÖB	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�#B	�;B	�;B	�5B	�;B	�;B	�BB	�HB	�BB	�/B	�#B	�5B	�HB	�HB	�;B	�5B	�/B	�5B	�;B	�BB	�BB	�)B	�
B	��B	�B	�B	�#B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�NB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
DB
�B
eB
$�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190511                              AO  ARCAADJP                                                                    20181005190511    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190511  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190511  QCF$                G�O�G�O�G�O�8000            