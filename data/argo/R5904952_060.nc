CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:18Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181005190518  20181005190518  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               <A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׾d[�e1   @׾d�s��@1LI�^5?�c��j~��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      <A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A���A�  A�33A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�33B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D y�D ��D� D  D�fDfD�fDfD� D  D� D  D�fDfD� D  D� D��D	� D
  D
� D  D� D  D� DfD� D  D�fD  D� D  D� D  D� DfD� DfD� D  D� D  D� D  D� D  D� DfD�fDfD� D��D� DfD�fD  D� D  D� D  Dy�D  D�fD fD � D!  D!� D"  D"� D#  D#�fD$  D$y�D$��D%y�D&  D&�fD'fD'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-fD-� D.  D.� D/  D/�fD0  D0�fD1fD1�fD2fD2� D2��D3y�D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8�fD9  D9� D:  DF  DF� DGfDGy�DH  DH� DH��DIy�DI��DJ� DK  DK� DLfDL�fDL��DM�fDNfDN� DO  DO� DP  DP� D\� D]fD]� D^  D^� D^��D_y�D`  D`�fDafDa� Db  Db� Dc  Dc� Dd  Dd� Dd��De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dl��Dmy�Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv��Dwy�Dw�fDy��D�4�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��R@��RA\)A?\)A_\)A\)A��A��A�z�A�z�AϮA��GA�A��B�
B�
B�
B�
B'�
B/p�B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB��B��B��B��C��C��C��C]C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CF]CG��CI��CK��CM��CO��CQ��CT]CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Cl]Cm��Co��Cq��Cs��Cu��Cw��Cz]C{��C}��C��C��C���C���C���C���C���C���C���C��C���C��C��C��C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C��C���C��C��C���C��C��C���C��C��C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C��C��C���C���C���C��C���C���C���C���C��C���C���C���C���C���C���C���D wD �D}qD�qD��D�D��D�D}qD�qD}qD�qD��D�D}qD�qD}qD�D	}qD	�qD
}qD
�qD}qD�qD}qD�D}qD�qD��D�qD}qD�qD}qD�qD}qD�D}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�D��D�D}qD�D}qD�D��D�qD}qD�qD}qD�qDwD�qD��D �D }qD �qD!}qD!�qD"}qD"�qD#��D#�qD$wD$�D%wD%�qD&��D'�D'��D'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,��D-�D-}qD-�qD.}qD.�qD/��D/�qD0��D1�D1��D2�D2}qD2�D3wD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7wD7�qD8��D8�qD9}qD9�qDE�qDF}qDG�DGwDG�qDH}qDH�DIwDI�DJ}qDJ�qDK}qDL�DL��DL�DM��DN�DN}qDN�qDO}qDO�qDP}qD\}qD]�D]}qD]�qD^}qD^�D_wD_�qD`��Da�Da}qDa�qDb}qDb�qDc}qDc�qDd}qDd�De}qDf�Df}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj��Dj�qDk}qDk�qDl}qDl�DmwDm�qDn��Dn�qDo}qDo�qDp}qDp�qDq��Dq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�DwwDw��Dy�HD�3�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AڶFAڶFAھwAھwA���A���A�A�AڼjAڧ�Aڲ-Aڥ�AڬAک�AړuA�M�A�`BA�7LA�$�A���A���AնFA�G�A��#A���A���A�|�A��A�I�A��AЧ�A�1'A�VA�hsA�|�A�l�A�~�A�A�Q�A�v�A��;A�|�A��A�%A�Aħ�A�VA��;A�$�A�A��A�+A¡�A��TA��\A��+A��!A�"�A��HA�dZA��TA�|�A�ZA��A��A��A��9A���A�ZA�r�A���A���A��
A��A�$�A���A���A�jA���A�%A��;A��PA���A�hsA�~�A��yA��A��;A�A�v�A���A�9XA�"�A��;A�r�A�K�A��DA�/A�E�A���A�ZA�r�A�|�A?}A}hsA{�^Av��At�As��Ar��Ap �Am"�Aj��Af~�AdbAa�A_�hA]�-A\�\AZM�AV��AUS�AQ��AN�AL�HAK;dAIAFr�AC��AA&�A?hsA>~�A=l�A<�A;�hA:�+A7�wA5hsA4~�A3hsA1�PA17LA/
=A,9XA+��A*VA(�A&n�A%�PA$��A$��A#A"��A!��A �jA JA�/A��AdZA��A�A�A�+A`BA�A�uA+AjA|�A�AQ�At�A�AjAjA�;AC�A(�A��A��A
=AA�-A�;A�A=qA~�AƨA	?}Ax�A5?A��A��AVA�wA �A ��A ��A n�@�E�@�hs@���@�~�@�hs@���@���@���@�9X@���@��@�@�+@�@�A�@�P@���@�@���@�\@�G�@�J@�$�@��@�Z@��m@��m@���@��
@�C�@�+@�7@㝲@�E�@�7L@��/@��@�1'@ߍP@��;@�@�j@݁@�|�@��H@��@ڇ+@�l�@�{@�O�@�9X@׮@�K�@֟�@�hs@�r�@�  @Դ9@�Q�@�1'@� �@���@���@�ff@���@�%@��@�C�@�$�@��/@˾w@˥�@˥�@ˍP@���@��@ȴ9@��@�bN@�j@ǅ@���@�=q@��@ź^@��/@�Z@�A�@�
=@°!@�E�@���@�Q�@���@�C�@��+@��7@���@��@��@���@���@�E�@�J@��7@�/@�&�@��@��`@��9@�b@�t�@�K�@�+@�33@��@�M�@��j@�j@�(�@�b@��;@��w@���@���@�n�@�ff@�^5@�=q@���@��u@��;@�+@��y@���@��@���@�J@��9@�  @���@���@���@�K�@�^5@���@�A�@��
@��;@��m@���@�K�@�K�@�K�@�+@���@��@��@���@��!@��@��!@�ff@�V@�5?@���@��-@�p�@��`@���@�r�@�t�@�5?@���@��7@�X@��@���@�Z@�b@��m@���@�33@�"�@��H@��!@���@�n�@�@���@���@��@��u@��@�p�@�`B@��@���@��D@�Z@�1'@�b@��@�|�@�S�@�C�@�33@��\@�?}@��@���@��j@�Z@��@��;@�l�@�"�@��y@���@�ȴ@�v�@���@�p�@�V@��@��9@���@�Q�@�1'@�b@��;@��@�dZ@�C�@��H@�E�@��@���@��7@�X@�7L@�G�@��@�O�@�&�@��@���@��@��@�  @�+@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111 AڶFAڶFAھwAھwA���A���A�A�AڼjAڧ�Aڲ-Aڥ�AڬAک�AړuA�M�A�`BA�7LA�$�A���A���AնFA�G�A��#A���A���A�|�A��A�I�A��AЧ�A�1'A�VA�hsA�|�A�l�A�~�A�A�Q�A�v�A��;A�|�A��A�%A�Aħ�A�VA��;A�$�A�A��A�+A¡�A��TA��\A��+A��!A�"�A��HA�dZA��TA�|�A�ZA��A��A��A��9A���A�ZA�r�A���A���A��
A��A�$�A���A���A�jA���A�%A��;A��PA���A�hsA�~�A��yA��A��;A�A�v�A���A�9XA�"�A��;A�r�A�K�A��DA�/A�E�A���A�ZA�r�A�|�A?}A}hsA{�^Av��At�As��Ar��Ap �Am"�Aj��Af~�AdbAa�A_�hA]�-A\�\AZM�AV��AUS�AQ��AN�AL�HAK;dAIAFr�AC��AA&�A?hsA>~�A=l�A<�A;�hA:�+A7�wA5hsA4~�A3hsA1�PA17LA/
=A,9XA+��A*VA(�A&n�A%�PA$��A$��A#A"��A!��A �jA JA�/A��AdZA��A�A�A�+A`BA�A�uA+AjA|�A�AQ�At�A�AjAjA�;AC�A(�A��A��A
=AA�-A�;A�A=qA~�AƨA	?}Ax�A5?A��A��AVA�wA �A ��A ��A n�@�E�@�hs@���@�~�@�hs@���@���@���@�9X@���@��@�@�+@�@�A�@�P@���@�@���@�\@�G�@�J@�$�@��@�Z@��m@��m@���@��
@�C�@�+@�7@㝲@�E�@�7L@��/@��@�1'@ߍP@��;@�@�j@݁@�|�@��H@��@ڇ+@�l�@�{@�O�@�9X@׮@�K�@֟�@�hs@�r�@�  @Դ9@�Q�@�1'@� �@���@���@�ff@���@�%@��@�C�@�$�@��/@˾w@˥�@˥�@ˍP@���@��@ȴ9@��@�bN@�j@ǅ@���@�=q@��@ź^@��/@�Z@�A�@�
=@°!@�E�@���@�Q�@���@�C�@��+@��7@���@��@��@���@���@�E�@�J@��7@�/@�&�@��@��`@��9@�b@�t�@�K�@�+@�33@��@�M�@��j@�j@�(�@�b@��;@��w@���@���@�n�@�ff@�^5@�=q@���@��u@��;@�+@��y@���@��@���@�J@��9@�  @���@���@���@�K�@�^5@���@�A�@��
@��;@��m@���@�K�@�K�@�K�@�+@���@��@��@���@��!@��@��!@�ff@�V@�5?@���@��-@�p�@��`@���@�r�@�t�@�5?@���@��7@�X@��@���@�Z@�b@��m@���@�33@�"�@��H@��!@���@�n�@�@���@���@��@��u@��@�p�@�`B@��@���@��D@�Z@�1'@�b@��@�|�@�S�@�C�@�33@��\@�?}@��@���@��j@�Z@��@��;@�l�@�"�@��y@���@�ȴ@�v�@���@�p�@�V@��@��9@���@�Q�@�1'@�b@��;@��@�dZ@�C�@��H@�E�@��@���@��7@�X@�7L@�G�@��@�O�@�&�@��@���@��@��@�  @�+@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�B
�B
�B
�B
�B
�B
uB
�B
=qB
[#B
YB
_;B
W
B
L�B
@�B
ZB
aHB
l�B
t�B
�bB
��B
�!B
ÖB
��B
�B{B,B<jBR�B[#Bl�Bs�B~�B�oB��B��B�!B�LBǮB��B�sB	7B5?BE�BK�BXBbNBm�B{�B�oB��B��B��B��B��B�
B�)B�NB�B��B�B�
B�'B��Bz�BZBA�BB��B�B�mBɺB�hBw�Bk�BT�BT�B`BBe`BQ�B5?B �B �B�BB
�5B
�RB
��B
��B
�hB
|�B
cTB
C�B
2-B
"�B
uB	��B	�B	�TB	�B	ɺB	�FB	��B	� B	w�B	k�B	cTB	ZB	P�B	=qB	0!B	8RB	-B	�B	JB��B��B�TB�B��B��BǮBĜBB��B�qB�?B�9B�LB�RB�FB�LB�^B��B��B�}B�dB�?B�!B�B��B��B��B��B��B��B��B��B�FBBƨBǮBȴB��B��B��B��B��B��B�B�B�TB��B	
=B	�B	�B	�B	�B	bB	JB	1B	B	\B	$�B	)�B	'�B	0!B	)�B	uB	%B��B��B�B�mB�B�yB�B�B�mB�;B�)B�
B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�yB��B��B�B	B		7B	1B	DB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	(�B	49B	>wB	C�B	@�B	=qB	>wB	A�B	L�B	\)B	ZB	_;B	_;B	aHB	cTB	bNB	_;B	]/B	`BB	l�B	m�B	n�B	q�B	r�B	s�B	v�B	x�B	x�B	x�B	x�B	x�B	w�B	z�B	}�B	~�B	~�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�+B	�%B	�B	�B	�%B	�B	�B	�B	�B	�B	�B	� B	}�B	}�B	� B	�B	�B	�B	�%B	�+B	�1B	�DB	�JB	�JB	�PB	�\B	�\B	�hB	�hB	�hB	�bB	�oB	��B	��B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�'B	�'B	�'B	�'B	�3B	�3B	�'B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�9B	�LB	�^B	�qB	�}B	��B	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	J�B	�BB	�BB	�BB	�BB	�5B	�/B	�)B	�)B	�)B	�/B	�;B	�HB	�NB	�TB	�TB	�TB	�NB	�NB	�ZB	�`B	�fB	�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B

=B
DB
JB
VB
\B
\B
\B
bB
bB
hB
hB
hB
hB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B

B
#�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224422222222222222222222222222222222222222222222222222222222222222222222222222222222 B
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
�B
�B
�B
�B
�B
�B
uB
�B
=qB
[#B
YB
_;B
W
B
L�B
@�B
ZB
aHB
l�B
t�B
�bB
��B
�!B
ÖB
��B
�B{B,B<jBR�B[#Bl�Bs�B~�B�oB��B��B�!B�LBǮB��B�sB	7B5?BE�BK�BXBbNBm�B{�B�oB��B��B��B��B��B�
B�)B�NB�B��B�B�
B�'B��Bz�BZBA�BB��B�B�mBɺB�hBw�Bk�BT�BT�B`BBe`BQ�B5?B �B �B�BB
�5B
�RB
��B
��B
�hB
|�B
cTB
C�B
2-B
"�B
uB	��B	�B	�TB	�B	ɺB	�FB	��B	� B	w�B	k�B	cTB	ZB	P�B	=qB	0!B	8RB	-B	�B	JB��B��B�TB�B��B��BǮBĜBB��B�qB�?B�9B�LB�RB�FB�LB�^B��B��B�}B�dB�?B�!B�B��B��B��B��B��B��B��B��B�FBBƨBǮBȴB��B��B��B��B��B��B�B�B�TB��B	
=B	�B	�B	�B	�B	bB	JB	1B	B	\B	$�B	)�B	'�B	0!B	)�B	uB	%B��B��B�B�mB�B�yB�B�B�mB�;B�)B�
B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�yB��B��B�B	B		7B	1B	DB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	(�B	49B	>wB	C�B	@�B	=qB	>wB	A�B	L�B	\)B	ZB	_;B	_;B	aHB	cTB	bNB	_;B	]/B	`BB	l�B	m�B	n�B	q�B	r�B	s�B	v�B	x�B	x�B	x�B	x�B	x�B	w�B	z�B	}�B	~�B	~�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�+B	�%B	�B	�B	�%B	�B	�B	�B	�B	�B	�B	� B	}�B	}�B	� B	�B	�B	�B	�%B	�+B	�1B	�DB	�JB	�JB	�PB	�\B	�\B	�hB	�hB	�hB	�bB	�oB	��B	��B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�'B	�'B	�'B	�'B	�3B	�3B	�'B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�9B	�LB	�^B	�qB	�}B	��B	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	J�B	�BB	�BB	�BB	�BB	�5B	�/B	�)B	�)B	�)B	�/B	�;B	�HB	�NB	�TB	�TB	�TB	�NB	�NB	�ZB	�`B	�fB	�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B

=B
DB
JB
VB
\B
\B
\B
bB
bB
hB
hB
hB
hB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B

B
#�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224422222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190518                              AO  ARCAADJP                                                                    20181005190518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190518  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190518  QCF$                G�O�G�O�G�O�C000            