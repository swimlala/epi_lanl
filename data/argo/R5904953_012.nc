CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:18Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190618  20181005190618  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @ץ���0`1   @ץ�Q�qr@3��`A�7�c���"��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @�33@�  A��A!��A@  A^ffA~ffA�  A�  A�  A�  A�  A���A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B���C  C  C�fC�fC	�fC�fC��C�fC  C�C�C�C�C�fC�fC   C"  C#�fC%�fC(  C*  C,  C.  C0�C2  C4  C6�C8�C:  C<  C>  C@  CB  CC�fCF�CH�CJ  CK�fCM�fCO�fCQ��CS��CV�CX�CY�fC\  C^  C`  Cb�Cd  Cf  Ch  Cj�Ck�fCm�fCp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C��C��3C�  C��3C��3C��C��C��C�  C�  C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��C��C�  C��C��C�  C�  C�  C�  C��3C��3C��fC��3C��3C��3C��3C��3C��C��C�&fC��C�  C��3C��3C��3C�  C��C�  C��3C��3C�  C��C�  C��3C��C�  C��3C��3C��fC��3C�  C�  C��3C�  C��C�  C��3C��3C��3C�  C��C��3C�  C��C�  C��3C��3C��C�  C��3C�  C��3C��3C��fC�  C��C��C��C��3C��fC��3C��3C��C��C��C�  C��3C��3C��3C��3C�  C��C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D y�D ��D� DfD� D��Dy�D��Dy�D  D�fD  D� D��Dy�D��D� D	  D	y�D
  D
� D  D� DfD�fDfD� D  D� D  D� D  Dy�D��DfD�fD��Dy�D  D� D   D �fD!  D!y�D!��D"� D#  D#� D$fD$�fD%fD%� D&  D&� D'  D'� D(fD(� D)  D)�fD*  D*y�D+  D+� D,  D,� D-fD-�fD.  D.� D/  D/y�D0  D0�fD1fD1� D2  D2y�D2�3D3y�D3��D4y�D5  D5� D5��D6� D7  D7� D8  D8y�D9  D9�fD:fD:y�D;  D;�fD<  D<y�D=  D=�fD>  D>y�D?  D?� D?��D@y�DA  DA� DBfDB� DC  DC�fDD  DDy�DE  DE�fDF  DF� DG  DG�fDH  DHy�DH��DI� DJ  DJ� DKfDK� DL  DLy�DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX�fDYfDY� DZ  DZ� D[  D[� D\fD\�fD]  D]y�D^  D^� D^��D_y�D_��D`� DafDa�fDbfDb� Db��Dc� Dd  Ddy�Dd��Dey�Df  Df�fDg  Dgy�DhfDh�fDi  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Duy�Dv  Dv� Dw  Dwy�Dw� Dy�qD�5qD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@ÅA\)A#\)AAA`(�A�{A��HA��HA��HA��HA��HA�A��HB �
Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bx�
B�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�B�8RC �C)C)C�C�C
�C�C��C�C)C5�C5�C5�C5�C�C�C )C")C$�C&�C()C*)C,)C.)C05�C2)C4)C65�C85�C:)C<)C>)C@)CB)CD�CF5�CH5�CJ)CL�CN�CP�CQ��CS��CV5�CX5�CZ�C\)C^)C`)Cb5�Cd)Cf)Ch)Cj5�Cl�Cn�Cp)Cr)Ct)Cv)Cx)Cz)C|�C~)C�C�C��C�GC�C�GC�GC��C��C��C�C�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�C�C�'�C�'�C��C�'�C�'�C�'�C��C�C��C��C�C�C�C�C�GC�GC��zC�GC�GC�GC�GC�GC�'�C�'�C�4zC��C�C�GC�GC�GC�C�'�C�C�GC�GC�C��C�C�GC��C�C�GC�GC��zC�GC�C�C�GC�C��C�C�GC�GC�GC�C��C�GC�C��C�C�GC�GC��C�C�GC�C�GC�GC��zC�C�'�C��C��C�GC��zC�GC�GC��C�'�C�'�C�C�GC�GC�GC�GC�C��C�C�GC�C��C�C�C�C��C�C�C�C�C�C�C��C�C�C�D 
D ��D �D�
DpD�
D �D��D �D��D
D�pD
D�
D �D��D �D�
D	
D	��D

D
�
D
D�
DpD�pDpD�
D
D�
D
D�
D
D��D �DpD�pD �D��D
D�
D 
D �pD!
D!��D" �D"�
D#
D#�
D$pD$�pD%pD%�
D&
D&�
D'
D'�
D(pD(�
D)
D)�pD*
D*��D+
D+�
D,
D,�
D-pD-�pD.
D.�
D/
D/��D0
D0�pD1pD1�
D2
D2��D2�=D3��D4 �D4��D5
D5�
D6 �D6�
D7
D7�
D8
D8��D9
D9�pD:pD:��D;
D;�pD<
D<��D=
D=�pD>
D>��D?
D?�
D@ �D@��DA
DA�
DBpDB�
DC
DC�pDD
DD��DE
DE�pDF
DF�
DG
DG�pDH
DH��DI �DI�
DJ
DJ�
DKpDK�
DL
DL��DM
DM�
DN
DN��DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS��DT
DT�
DU
DU��DV
DV�
DW
DW�
DX
DX�pDYpDY�
DZ
DZ�
D[
D[�
D\pD\�pD]
D]��D^
D^�
D_ �D_��D` �D`�
DapDa�pDbpDb�
Dc �Dc�
Dd
Dd��De �De��Df
Df�pDg
Dg��DhpDh�pDi
Di�
DjpDj�
Dk
Dk�
Dl
Dl�
Dm
Dm��Dn
Dn�
Do �Do�
Dp
Dp�
Dq
Dq�
Dr
Dr�
Ds
Ds�
Dt
Dt�
Du �Du��Dv
Dv�
Dw
Dw��Dw�
Dy�{D�8�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AʃAʃAʅAʅAʅAʇ+Aʇ+Aʇ+Aʉ7AʋDAʉ7Aʉ7Aʉ7AʍPAʕ�Aʙ�Aʙ�Aʧ�AʬAʑhAɅA�bA�n�A��mA��A�ZA��A�O�A�33A�~�A�p�A��DA�+A�ffA�ȴA�r�A��jA��A��A�1A��wA�-A��TA�(�A�I�A��PA���A�A�A���A��#A�33A�A��hA���A���A�+A�v�A�"�A��A�l�A��A��A�\)A��^A�33A�p�A��jA���A�z�A��hA���A�(�A��
A�~�A�;dA�1A���A��A��A���A�
=A���A��^AG�A~^5A}A|�Az�RAxE�Au�7AsS�Arv�Ap��Ao�PAm�TAmdZAlZAgXAe33Ac��Aa�-A\~�AYAV��ATjAQ`BANE�AL=qAJ(�AFv�AB��ABv�AAx�A@n�A>JA=
=A;p�A9�mA81'A6�/A5��A4bNA2Q�A1�hA1XA0�HA/ƨA/&�A.��A.(�A+��A)�;A&��A%��A$��A#�
A"�uA!G�A��A  A��A��AVAbA�A�PA��AoA�`A��A`BAG�A��AoA��AbNA�A�TA�AdZA�HA��A��A��A�PA�A
I�A	S�A��A��A��Ap�A^5A�A�AC�AQ�AS�A �A J@�l�@���@��@���@�n�@�G�@�l�@��h@���@��H@�@�V@�+@��@���@�
=@�A�@��@��
@�dZ@�=q@߶F@ް!@�E�@�/@�%@���@�z�@�+@ڸR@�n�@٩�@�ƨ@�ȴ@���@��m@���@�o@�@�(�@�dZ@ϥ�@�S�@�
=@�V@��#@�?}@�9X@�;d@�ȴ@�E�@��T@��@��T@ə�@��@ȴ9@�b@ǅ@�-@�?}@��/@Õ�@��@�/@��@��@��@�1@��@�-@��h@�7L@��@�(�@��P@�S�@�ff@�O�@��@��@��@��@��j@��D@�bN@�b@�ƨ@��H@�n�@�V@�@�O�@��9@��
@�+@�n�@�E�@�5?@���@�G�@��@��/@���@�bN@�1@��m@�ƨ@�\)@��@�A�@��@��m@��;@�  @�b@�(�@�A�@�(�@���@��@�|�@�dZ@�C�@�"�@�@��H@���@�@��^@�X@��@��j@��u@��u@�Q�@�b@�1@�|�@��y@��@��R@���@��+@�^5@�M�@�{@��@�hs@�&�@��9@�A�@� �@�1@��@�t�@�+@���@���@��+@��@�7L@���@��j@�bN@�(�@��m@��w@��@�t�@�33@��@�^5@�J@��@��#@��^@��h@��@�hs@�&�@�Ĝ@�(�@��F@��@�ȴ@�5?@�$�@��@��@��@��@��h@�`B@�X@�O�@��@���@���@�Ĝ@���@�I�@�b@�ƨ@���@�S�@��y@��R@�ff@��T@��7@��@��/@��@�j@� �@�  @��;@���@���@�ƨ@���@�dZ@�+@��@��@���@���@��!@���@�~�@�^5@��#@��7@�7L@�Ĝ@��D@�bN@�(�@��
@��P@��!@�$�@���@�@��-@��7@�?}@��`@���@�r�@�A�@�b@��m@�ƨ@��w@���@�l�@�"�@���@���@���@���@���@���@���@��`@�9X@�9X@�9X@� �@��@���@��F@��@�K�@�
=@���@�~�@�=q@��@��@��T@�hs@���@�9X@� �@�@K�@
=@
=@
=@�@o i@[dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AʃAʃAʅAʅAʅAʇ+Aʇ+Aʇ+Aʉ7AʋDAʉ7Aʉ7Aʉ7AʍPAʕ�Aʙ�Aʙ�Aʧ�AʬAʑhAɅA�bA�n�A��mA��A�ZA��A�O�A�33A�~�A�p�A��DA�+A�ffA�ȴA�r�A��jA��A��A�1A��wA�-A��TA�(�A�I�A��PA���A�A�A���A��#A�33A�A��hA���A���A�+A�v�A�"�A��A�l�A��A��A�\)A��^A�33A�p�A��jA���A�z�A��hA���A�(�A��
A�~�A�;dA�1A���A��A��A���A�
=A���A��^AG�A~^5A}A|�Az�RAxE�Au�7AsS�Arv�Ap��Ao�PAm�TAmdZAlZAgXAe33Ac��Aa�-A\~�AYAV��ATjAQ`BANE�AL=qAJ(�AFv�AB��ABv�AAx�A@n�A>JA=
=A;p�A9�mA81'A6�/A5��A4bNA2Q�A1�hA1XA0�HA/ƨA/&�A.��A.(�A+��A)�;A&��A%��A$��A#�
A"�uA!G�A��A  A��A��AVAbA�A�PA��AoA�`A��A`BAG�A��AoA��AbNA�A�TA�AdZA�HA��A��A��A�PA�A
I�A	S�A��A��A��Ap�A^5A�A�AC�AQ�AS�A �A J@�l�@���@��@���@�n�@�G�@�l�@��h@���@��H@�@�V@�+@��@���@�
=@�A�@��@��
@�dZ@�=q@߶F@ް!@�E�@�/@�%@���@�z�@�+@ڸR@�n�@٩�@�ƨ@�ȴ@���@��m@���@�o@�@�(�@�dZ@ϥ�@�S�@�
=@�V@��#@�?}@�9X@�;d@�ȴ@�E�@��T@��@��T@ə�@��@ȴ9@�b@ǅ@�-@�?}@��/@Õ�@��@�/@��@��@��@�1@��@�-@��h@�7L@��@�(�@��P@�S�@�ff@�O�@��@��@��@��@��j@��D@�bN@�b@�ƨ@��H@�n�@�V@�@�O�@��9@��
@�+@�n�@�E�@�5?@���@�G�@��@��/@���@�bN@�1@��m@�ƨ@�\)@��@�A�@��@��m@��;@�  @�b@�(�@�A�@�(�@���@��@�|�@�dZ@�C�@�"�@�@��H@���@�@��^@�X@��@��j@��u@��u@�Q�@�b@�1@�|�@��y@��@��R@���@��+@�^5@�M�@�{@��@�hs@�&�@��9@�A�@� �@�1@��@�t�@�+@���@���@��+@��@�7L@���@��j@�bN@�(�@��m@��w@��@�t�@�33@��@�^5@�J@��@��#@��^@��h@��@�hs@�&�@�Ĝ@�(�@��F@��@�ȴ@�5?@�$�@��@��@��@��@��h@�`B@�X@�O�@��@���@���@�Ĝ@���@�I�@�b@�ƨ@���@�S�@��y@��R@�ff@��T@��7@��@��/@��@�j@� �@�  @��;@���@���@�ƨ@���@�dZ@�+@��@��@���@���@��!@���@�~�@�^5@��#@��7@�7L@�Ĝ@��D@�bN@�(�@��
@��P@��!@�$�@���@�@��-@��7@�?}@��`@���@�r�@�A�@�b@��m@�ƨ@��w@���@�l�@�"�@���@���@���@���@���@���@���@��`@�9X@�9X@�9X@� �@��@���@��F@��@�K�@�
=@���@�~�@�=q@��@��@��T@�hs@���@�9X@� �@�@K�@
=@
=@
=@�@o i@[dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bz�B}�B{�B{�B|�B� B�+B�DB�JB��B��B��B��B��B�9B�qB��BB�B�B;dBM�BcTB|�B�=B�uB��B��B��B��B�oB�1B�B{�Bu�Br�Bm�BgmBaHBS�BI�BC�B;dB-B-BJ�BYBXBD�B.BJB��B��B�B�)B��B�7Be`BI�B?}BC�BF�B?}B.B/B(�B%�BB
�HB
�
B
��B
�RB
��B
��B
~�B
`BB
^5B
S�B
R�B
J�B
9XB
�B
B	��B	�fB	�)B	��B	�B	�)B	�jB	��B	��B	�+B	jB	YB	F�B	2-B	�B	VB	B��B�HB��B��BǮB�}B�LB�-B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B�'B�-B�!B�B�B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�\B�=B�+B�JB�JB�=B�B~�B� B~�B� B� B� B�B�B� B�B�1B�1B�+B�1B�JB�\B�\B�VB�VB�VB�VB�VB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�9B�?B�?B�^B��BɺB��B��B��B��B��B�B�B�B�)B�/B�/B�/B�5B�BB�HB�TB�`B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	
=B	DB	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	(�B	)�B	)�B	+B	/B	2-B	5?B	:^B	;dB	<jB	>wB	A�B	B�B	D�B	E�B	F�B	I�B	J�B	K�A��^B	x�B	z�B	|�B	~�B	�B	�%B	�1B	�7B	�=B	�JB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�3B	�3B	�9B	�?B	�?B	�FB	�RB	�RB	�dB	�jB	�qB	�wB	�wB	�qB	�wB	�wB	�}B	�}B	��B	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
1B
	7B
1B
	7B
	7B

=B
JB
PB
PB
PB
PB
VB
PB
PB
PB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
�B
#nB
3�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bz�B}�B{�B{�B|�B� B�+B�DB�JB��B��B��B��B��B�9B�qB��BB�B�B;dBM�BcTB|�B�=B�uB��B��B��B��B�oB�1B�B{�Bu�Br�Bm�BgmBaHBS�BI�BC�B;dB-B-BJ�BYBXBD�B.BJB��B��B�B�)B��B�7Be`BI�B?}BC�BF�B?}B.B/B(�B%�BB
�HB
�
B
��B
�RB
��B
��B
~�B
`BB
^5B
S�B
R�B
J�B
9XB
�B
B	��B	�fB	�)B	��B	�B	�)B	�jB	��B	��B	�+B	jB	YB	F�B	2-B	�B	VB	B��B�HB��B��BǮB�}B�LB�-B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B�'B�-B�!B�B�B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�\B�=B�+B�JB�JB�=B�B~�B� B~�B� B� B� B�B�B� B�B�1B�1B�+B�1B�JB�\B�\B�VB�VB�VB�VB�VB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�9B�?B�?B�^B��BɺB��B��B��B��B��B�B�B�B�)B�/B�/B�/B�5B�BB�HB�TB�`B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	
=B	DB	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	(�B	)�B	)�B	+B	/B	2-B	5?B	:^B	;dB	<jB	>wB	A�B	B�B	D�B	E�B	F�B	I�B	J�B	K�A��^B	x�B	z�B	|�B	~�B	�B	�%B	�1B	�7B	�=B	�JB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�3B	�3B	�9B	�?B	�?B	�FB	�RB	�RB	�dB	�jB	�qB	�wB	�wB	�qB	�wB	�wB	�}B	�}B	��B	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
1B
	7B
1B
	7B
	7B

=B
JB
PB
PB
PB
PB
VB
PB
PB
PB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
�B
#nB
3�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190618                              AO  ARCAADJP                                                                    20181005190618    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190618  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190618  QCF$                G�O�G�O�G�O�8000            