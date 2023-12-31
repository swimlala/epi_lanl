CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:04Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191704  20181005191704  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               FA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�����
�1   @���q�/&@5K��Q��d     1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      FA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B���B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C�C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CA�fCC�fCE�fCG�fCI�fCK�fCN  CP�CR�CT  CV�CX  CY�fC\  C]�fC_�fCb  Cd�Cf  Cg�fCj  Cl  Cm�fCo�fCr  Ct  Cv  Cx�Cz�C|  C}�fC�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C��C��C�  C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C��fC�  C�  C�  C��C�  C�  C�  C��C��C��C�  C�  C��C�  C��3C�  C��3C��3C��C�  C��3C�  C�  C��fC�  C��3C��3C��3C��3C�  C��C��C��C�  C�  C�  C�  C��3C��3C��C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C��3C��C��3C��3C��3C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3D � D  D� D��D� D  D� D  D�fD��D�fDfDy�D  D� DfDy�D	  D	y�D
  D
� D  D�fD  D�fDfD� D��Dy�D��D� DfD� DfDy�D��D� DfD� D  D� D  D� D  D� D  Dy�D  Dy�D  D� D��D�fD  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D$��D%�fD&  D&� D'  D'� D(fD(� D)  D)y�D*fD*� D+  D+�fD+��D,�fD-fD-y�D.fD.� D/fD/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6y�D7  D7�fD8  D8�fD9  D9� D:  D:� D;fD;y�D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DCfDCy�DD  DDy�DE  DE� DF  DF� DGfDG� DG��DH� DI  DI� DI��DJ� DK  DK� DLfDL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DX��DYy�DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Day�Db  Db� DcfDcy�Dd  Dd� Dd��De�fDe��Df� Dg  Dg� Dg��Dh� Dh��Di�fDi��Dj� DkfDk� Dl  Dly�Dl��Dm� Dm��Dn� DofDoy�DpfDpy�Dp��Dq�fDr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dw��Dy��D�FfD��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bx�
B�8RB�8RB�8RB�8RB�B�B�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�B�8RB�k�B�8RB�8RB�8RC )C)C)C)C)C
)C)C)C5�C5�C)C5�C)C)C�C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<5�C>)C@)CB�CD�CF�CH�CJ�CL�CN)CP5�CR5�CT)CV5�CX)CZ�C\)C^�C`�Cb)Cd5�Cf)Ch�Cj)Cl)Cn�Cp�Cr)Ct)Cv)Cx5�Cz5�C|)C~�C�C�C��C�C�GC�C�C�C�C�C��C�C�C�C�C��C�C�C�C�GC��C��C�C�GC�C�C�C�GC�C��C�C�C�C�GC�C�C��zC�C�C�C��C�C�C�C��C��C��C�C�C��C�C�GC�C�GC�GC��C�C�GC�C�C��zC�C�GC�GC�GC�GC�C��C��C��C�C�C�C�C�GC�GC��C�'�C�C�C�C�C��C�C�GC�C�C�C�C�GC�C�GC�C�C�C�GC��C�GC�GC�GC��C��C�C�C��C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C��C�C�D  �D �
D
D�
D �D�
D
D�
D
D�pD �D�pDpD��D
D�
DpD��D	
D	��D

D
�
D
D�pD
D�pDpD�
D �D��D �D�
DpD�
DpD��D �D�
DpD�
D
D�
D
D�
D
D�
D
D��D
D��D
D�
D �D�pD
D�pD
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"pD"�
D#
D#�
D$
D$�
D% �D%�pD&
D&�
D'
D'�
D(pD(�
D)
D)��D*pD*�
D+
D+�pD, �D,�pD-pD-��D.pD.�
D/pD/�
D0
D0�
D1pD1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�pD6
D6��D7
D7�pD8
D8�pD9
D9�
D:
D:�
D;pD;��D<
D<�
D=
D=�pD>
D>�
D?
D?�
D@
D@�
DA
DA�
DB �DB�
DCpDC��DD
DD��DE
DE�
DF
DF�
DGpDG�
DH �DH�
DI
DI�
DJ �DJ�
DK
DK�
DLpDL�pDM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DVpDV�
DW
DW�
DX
DX�
DY �DY��DZ
DZ�
D[ �D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_��D`
D`�
Da
Da��Db
Db�
DcpDc��Dd
Dd�
De �De�pDf �Df�
Dg
Dg�
Dh �Dh�
Di �Di�pDj �Dj�
DkpDk�
Dl
Dl��Dm �Dm�
Dn �Dn�
DopDo��DppDp��Dq �Dq�pDr
Dr�
Ds
Ds�
Dt
Dt�pDu
Du�
Dv
Dv�
Dw
Dw�
Dw��Dy��D�I�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�I�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�VA�ZA�XA�O�A�%A�"�A�(�AЛ�AάA�A�$�A�5?A�AʑhA�/A�dZA��A�bNAƏ\A�M�A���AuA�$�A�  A�$�A�9XA�|�A��`A��/A���A�%A��`A��A�O�A��mA��A��7A�v�A�hsA�I�A�$�A�ffA���A�  A�^5A�1'A�{A�A�ĜA�?}A��A���A�&�A��A�A���A�I�A��A��+A�oA�bNA�A�`BA�bA�n�A�"�A��/A��`A��A��A���A��!A��mA�A��A��PA�bNA�VA�ȴA�ȴA�ȴA���A�%A�v�A�7A|�uAz�RAy��Ax��Aw�^AvQ�Au�At9XAsx�Ar$�AoAmhsAjJAiAh9XAgt�Ad��Ab�Abn�Aa�;AaA_�PA[�AUVARjAQ��AO�mAM�wAKS�AH�\AG|�AGoAF��AFjAFE�AE�TAEdZADA�AA��A@~�A?33A=�hA<1A;x�A:$�A81A7VA6�`A6I�A5��A3O�A0�A/"�A.I�A-|�A-VA,�A,$�A+�A*��A*��A*~�A*bNA*ffA*=qA(��A&I�A%"�A$VA#l�A#�A"��A"��A"�+A"E�A!�mA E�AE�A^5A��AZA�RAO�A�A(�A(�A �A�AA�9AAA�A��A��A�A	�;A-AXA�!A��A�AM�A+A�mAƨA�wA��AXA ��A Q�A �A �j@��@�Z@��@�Ĝ@��@�(�@�w@�ff@���@�(�@�1@���@���@�!@�Ĝ@�^5@���@��
@�;d@�+@�K�@��#@߮@���@�S�@��y@ޟ�@�^5@�n�@�~�@ݲ-@ݡ�@݉7@�X@�O�@���@ݡ�@�bN@ۮ@�+@�E�@�hs@�j@�t�@�"�@֧�@�E�@ԣ�@��y@�@��@Ь@� �@�C�@�@�o@�^5@̛�@�(�@˾w@�\)@��@�V@�hs@�Ĝ@ǝ�@��#@�A�@���@��@��-@�hs@�%@���@�j@�1@��@�~�@��@���@�O�@�/@��9@���@�dZ@�C�@�@��+@��-@�X@���@��@��@�ff@��-@��j@�r�@�r�@�z�@�r�@�bN@�b@��F@���@��@�|�@�\)@���@��@�X@�V@�I�@��
@�dZ@�\)@�o@��R@�^5@��@��@�O�@�%@�9X@�S�@��y@�ff@�=q@�J@���@��h@�`B@�V@��j@�j@�1@�t�@�n�@�~�@���@���@�G�@��`@�r�@�A�@��@�33@�n�@�~�@���@��@�~�@��@�@���@�O�@��@�1@� �@�ƨ@�t�@�l�@�dZ@�dZ@�C�@��@���@��R@��!@�n�@�V@�E�@�@���@�O�@��@��@�1'@�(�@�(�@��@��
@�t�@��H@��R@��!@���@���@�n�@�M�@�V@�ff@�ff@�E�@�=q@���@���@�/@��/@��9@�&�@��@�1'@�(�@� �@� �@��;@��@�C�@�@��y@��R@��+@�V@��@��^@��7@�p�@�`B@�G�@���@��u@�bN@�(�@���@��@���@���@�dZ@�o@��H@��!@���@���@��+@�n�@�V@��@���@��^@���@�p�@�hs@�&�@�Ĝ@�bN@�I�@�1'@��
@��@���@���@���@���@�~�@�^5@�{@��^@�`B@�%@�r�@�1'@���@��@���@�+@��R@�V@�=q@�-@���@�?}@�V@���@��/@��9@�bN@�b@��;@���@��@�l�@�33@���@��H@���@�ȴ@�_�@}��@h9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�I�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�VA�ZA�XA�O�A�%A�"�A�(�AЛ�AάA�A�$�A�5?A�AʑhA�/A�dZA��A�bNAƏ\A�M�A���AuA�$�A�  A�$�A�9XA�|�A��`A��/A���A�%A��`A��A�O�A��mA��A��7A�v�A�hsA�I�A�$�A�ffA���A�  A�^5A�1'A�{A�A�ĜA�?}A��A���A�&�A��A�A���A�I�A��A��+A�oA�bNA�A�`BA�bA�n�A�"�A��/A��`A��A��A���A��!A��mA�A��A��PA�bNA�VA�ȴA�ȴA�ȴA���A�%A�v�A�7A|�uAz�RAy��Ax��Aw�^AvQ�Au�At9XAsx�Ar$�AoAmhsAjJAiAh9XAgt�Ad��Ab�Abn�Aa�;AaA_�PA[�AUVARjAQ��AO�mAM�wAKS�AH�\AG|�AGoAF��AFjAFE�AE�TAEdZADA�AA��A@~�A?33A=�hA<1A;x�A:$�A81A7VA6�`A6I�A5��A3O�A0�A/"�A.I�A-|�A-VA,�A,$�A+�A*��A*��A*~�A*bNA*ffA*=qA(��A&I�A%"�A$VA#l�A#�A"��A"��A"�+A"E�A!�mA E�AE�A^5A��AZA�RAO�A�A(�A(�A �A�AA�9AAA�A��A��A�A	�;A-AXA�!A��A�AM�A+A�mAƨA�wA��AXA ��A Q�A �A �j@��@�Z@��@�Ĝ@��@�(�@�w@�ff@���@�(�@�1@���@���@�!@�Ĝ@�^5@���@��
@�;d@�+@�K�@��#@߮@���@�S�@��y@ޟ�@�^5@�n�@�~�@ݲ-@ݡ�@݉7@�X@�O�@���@ݡ�@�bN@ۮ@�+@�E�@�hs@�j@�t�@�"�@֧�@�E�@ԣ�@��y@�@��@Ь@� �@�C�@�@�o@�^5@̛�@�(�@˾w@�\)@��@�V@�hs@�Ĝ@ǝ�@��#@�A�@���@��@��-@�hs@�%@���@�j@�1@��@�~�@��@���@�O�@�/@��9@���@�dZ@�C�@�@��+@��-@�X@���@��@��@�ff@��-@��j@�r�@�r�@�z�@�r�@�bN@�b@��F@���@��@�|�@�\)@���@��@�X@�V@�I�@��
@�dZ@�\)@�o@��R@�^5@��@��@�O�@�%@�9X@�S�@��y@�ff@�=q@�J@���@��h@�`B@�V@��j@�j@�1@�t�@�n�@�~�@���@���@�G�@��`@�r�@�A�@��@�33@�n�@�~�@���@��@�~�@��@�@���@�O�@��@�1@� �@�ƨ@�t�@�l�@�dZ@�dZ@�C�@��@���@��R@��!@�n�@�V@�E�@�@���@�O�@��@��@�1'@�(�@�(�@��@��
@�t�@��H@��R@��!@���@���@�n�@�M�@�V@�ff@�ff@�E�@�=q@���@���@�/@��/@��9@�&�@��@�1'@�(�@� �@� �@��;@��@�C�@�@��y@��R@��+@�V@��@��^@��7@�p�@�`B@�G�@���@��u@�bN@�(�@���@��@���@���@�dZ@�o@��H@��!@���@���@��+@�n�@�V@��@���@��^@���@�p�@�hs@�&�@�Ĝ@�bN@�I�@�1'@��
@��@���@���@���@���@�~�@�^5@�{@��^@�`B@�%@�r�@�1'@���@��@���@�+@��R@�V@�=q@�-@���@�?}@�V@���@��/@��9@�bN@�b@��;@���@��@�l�@�33@���@��H@���@�ȴ@�_�@}��@h9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3BȴB�B�)B�yB�B�B��B%B\BhB�B �B.B>wBH�BS�B]/Bo�Bt�Bx�B�1B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�VB�Bk�B]/BZBXBR�BK�B;dB�BJBB�HB��B��B�{Br�BaHBF�B �B+B
�B
�fB
�ZB
�TB
�BB
�/B
��B
�qB
�\B
o�B
aHB
[#B
Q�B
F�B
7LB
,B
#�B
�B
�B
PB
%B
B	��B	��B	�fB	�
B	�wB	�FB	�!B	��B	��B	�PB	�7B	�B	|�B	q�B	VB	49B	&�B	"�B	�B	VB	B��B�B�B�B�B�B�B�B�fB�HB�5B�)B�B��B��B��BƨBÖBÖBB�wB�?B�B�B�B�B�B�B�B�B��B��B��B�B�B�B�B��B�B�-B�?B�9B�3B�-B�'B�B�B��B��B�{B��B�{B�bB�%B�B�B�=B�=B�7B�=B�1B|�Bz�Bt�Bu�BiyBR�BVBI�BA�B?}B>wB>wB=qB@�BH�BI�BI�BJ�BL�BS�BXB_;BiyBhsBe`BbNBdZBiyBo�Br�Bu�Bx�Bx�Bx�Bx�Bw�Bv�B{�By�Bw�Bv�Bv�Bv�Bv�Bu�Bt�By�B~�B~�B~�B~�B�1B��B�{B�uB�{B��B��B��B��B��B��B��B�B�B�!B�-B�3B�FB�LB�dBĜBȴBȴBɺB��B��B��B��B��B�B�B�#B�)B�5B�BB�NB�`B�ZB�NB�`B�mB�yB�yB�yB�B�B�B�B�B��B��B��B��B��B��B	B	%B	%B	1B	DB	\B	bB	uB	�B	�B	�B	#�B	'�B	)�B	)�B	)�B	+B	,B	1'B	49B	5?B	6FB	6FB	7LB	;dB	?}B	A�B	B�B	D�B	F�B	I�B	O�B	Q�B	S�B	VB	W
B	\)B	^5B	^5B	dZB	gmB	hsB	k�B	l�B	m�B	o�B	q�B	s�B	u�B	x�B	x�B	|�B	|�B	}�B	�B	�B	�DB	�PB	�PB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�FB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�jB	�qB	�qB	�qB	�qB	�wB	��B	ÖB	ĜB	ǮB	ȴB	ȴB	ȴB	ǮB	ƨB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
	7B
	�B
�B
)_222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3BȴB�B�)B�yB�B�B��B%B\BhB�B �B.B>wBH�BS�B]/Bo�Bt�Bx�B�1B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�VB�Bk�B]/BZBXBR�BK�B;dB�BJBB�HB��B��B�{Br�BaHBF�B �B+B
�B
�fB
�ZB
�TB
�BB
�/B
��B
�qB
�\B
o�B
aHB
[#B
Q�B
F�B
7LB
,B
#�B
�B
�B
PB
%B
B	��B	��B	�fB	�
B	�wB	�FB	�!B	��B	��B	�PB	�7B	�B	|�B	q�B	VB	49B	&�B	"�B	�B	VB	B��B�B�B�B�B�B�B�B�fB�HB�5B�)B�B��B��B��BƨBÖBÖBB�wB�?B�B�B�B�B�B�B�B�B��B��B��B�B�B�B�B��B�B�-B�?B�9B�3B�-B�'B�B�B��B��B�{B��B�{B�bB�%B�B�B�=B�=B�7B�=B�1B|�Bz�Bt�Bu�BiyBR�BVBI�BA�B?}B>wB>wB=qB@�BH�BI�BI�BJ�BL�BS�BXB_;BiyBhsBe`BbNBdZBiyBo�Br�Bu�Bx�Bx�Bx�Bx�Bw�Bv�B{�By�Bw�Bv�Bv�Bv�Bv�Bu�Bt�By�B~�B~�B~�B~�B�1B��B�{B�uB�{B��B��B��B��B��B��B��B�B�B�!B�-B�3B�FB�LB�dBĜBȴBȴBɺB��B��B��B��B��B�B�B�#B�)B�5B�BB�NB�`B�ZB�NB�`B�mB�yB�yB�yB�B�B�B�B�B��B��B��B��B��B��B	B	%B	%B	1B	DB	\B	bB	uB	�B	�B	�B	#�B	'�B	)�B	)�B	)�B	+B	,B	1'B	49B	5?B	6FB	6FB	7LB	;dB	?}B	A�B	B�B	D�B	F�B	I�B	O�B	Q�B	S�B	VB	W
B	\)B	^5B	^5B	dZB	gmB	hsB	k�B	l�B	m�B	o�B	q�B	s�B	u�B	x�B	x�B	|�B	|�B	}�B	�B	�B	�DB	�PB	�PB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�FB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�jB	�qB	�qB	�qB	�qB	�wB	��B	ÖB	ĜB	ǮB	ȴB	ȴB	ȴB	ǮB	ƨB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
	7B
	�B
�B
)_222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191704                              AO  ARCAADJP                                                                    20181005191704    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191704  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191704  QCF$                G�O�G�O�G�O�8000            