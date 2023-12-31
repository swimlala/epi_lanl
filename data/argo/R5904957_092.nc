CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:21Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140821  20181024140821  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               \A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�ƥ�Z1   @�ƥ�n�@3���`A��c�n��P1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      \A   A   A   @9��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fDfD�fD  D� D  D�fD  D� D  D� D  D� D  D� D  D� D��Dy�D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&y�D'  D'� D(  D(y�D)  D)�fD*  D*� D+  D+� D,  D,y�D-  D-� D-��D.� D/  D/� D/��D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DEfDE�fDF  DF� DG  DGy�DG��DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN�fDOfDO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\� D]  D]� D^fD^� D_  D_� D`  D`� DafDa�fDb  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDw�fDy�HD�K�D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@��@��R@ÅAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BX�
B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,�C.)C0)C2)C4)C6)C8)C:�C<)C>)C@)CB�CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch5�Cj)Cl)Cn)Cp)Cr5�Ct5�Cv)Cx)Cz)C|)C~)C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C��C��C�C�GC�GC�GC�C�C�C�C�C�C�C�C�GC�GC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�GC�GC�GC�GC�GC�C�C�GC�GC�C�C�C�C�D pD �
D
D�
D
D�
D
D�
D
D�pDpD�
D
D�
D
D�
D
D�
D	
D	�
D

D
�
D
D�
D
D�
D
D�pDpD�pD
D�
D
D�pD
D�
D
D�
D
D�
D
D�
D
D�
D �D��D
D�
DpD�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D��D
D�pD 
D �
D!
D!�
D"
D"�
D#
D#�pD$
D$�
D%
D%�
D&
D&��D'
D'�
D(
D(��D)
D)�pD*
D*�
D+
D+�
D,
D,��D-
D-�
D. �D.�
D/
D/�
D0 �D0�
D1pD1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DA
DA�
DB
DB��DC
DC�
DD
DD�
DEpDE�pDF
DF�
DG
DG��DH �DH�
DI
DI�
DJ
DJ�
DKpDK�
DL
DL�
DM
DM�
DN
DN�pDOpDO�
DP
DP�
DQpDQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DV
DV�
DW
DW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\ �D\�
D]
D]�
D^pD^�
D_
D_�
D`
D`�
DapDa�pDb
Db�
Dc
Dc�
Dd
Dd�
De
De�pDf
Df�
Dg
Dg�
Dh �Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dl
Dl�
Dm
Dm�
Dn
Dn��Do �Do�
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
Du
Du�
Dv
Dv�
Dw
Dw�pDw�pDy�RD�O
D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A�$�A�&�A� �A� �A� �A��A�
=A���A���A���A���A���A���A��mA�~�A�7LAܣ�AۃA���A�oA٩�A�dZA���A�I�A�9XA���AΧ�A̸RA���A�
=Aʕ�A�XA���A�"�A�|�A�=qAÏ\A�~�A�ZA���A�bNA� �A��A�G�A�l�A���A��TA��A�VA�JA�K�A�A�A��hA�33A�bNA��jA���A��A���A�VA���A�ZA��A�oA�G�A�33A��A�=qA���A��hA�jA�
=A�7LA�^5A�C�A��A�\)A�;dA��`A��A�^5A�1'A�&�A�ffA�{A�t�A�1A���A���A��HA�hsA�x�A��wA�+A��RA��HA�r�A���A�1A��PA�n�A�hsA�Q�A~��Az�\AxI�Av��Au�#AtQ�Ar�/Ap��Anz�Ai%AfZAd�Ad�Abv�Ab^5Ab�Ab��Ab�A`ĜA\E�AZ��AX�jAV��AU�hAU&�ATbNARbNAK�mAJ�uAHr�AB1A>5?A9��A4bNA1l�A/��A/�A.��A.�A.�DA-�A)�mA'��A&(�A%��A$��A#S�A"��A"A�A!��AhsAz�A|�A�A�uA�!A�A��Ar�A-A`BA��A��A(�A\)A��AQ�AAC�A�AI�Ax�A��A�/A�A�PA
�A��A�AjA�hAVAQ�A�9AK�A   @�&�@�z�@�E�@���@��D@�A�@�(�@��m@�-@�bN@띲@�~�@�/@��m@�33@�$�@�`B@�%@���@�Z@��
@�33@��y@���@�7@���@���@���@�A�@�\)@ٲ-@ְ!@�dZ@���@�1@��@�~�@͑h@�7L@�hs@�?}@̬@���@��@���@Ȭ@��@�K�@�5?@�%@ă@�bN@�I�@��@�  @��
@���@�dZ@���@��-@�Z@�1@��P@�t�@�l�@�\)@�ȴ@�v�@�M�@��#@�@��^@���@��@��h@�+@���@��@�p�@�&�@���@�o@���@��+@��\@��\@�~�@�v�@��h@�/@��@��`@�  @�33@�E�@��h@���@���@�o@��@���@��@���@���@�ȴ@���@�@���@��`@��/@��9@�r�@�(�@���@�o@�@��T@�@���@��@�hs@�hs@�hs@�`B@�?}@�7L@��@�Ĝ@�9X@��
@���@�C�@���@�M�@�@�hs@��@���@���@�r�@�(�@��;@�K�@���@�n�@�^5@�=q@��#@�/@�(�@��P@��!@��+@�ff@��@���@��@���@��9@�Z@��@�1@�  @���@��F@���@���@��P@�o@��R@�v�@�=q@�-@��@��T@��-@��7@�p�@�X@�?}@��@���@�Ĝ@��@�A�@� �@��@��
@���@�l�@�+@�o@��@��R@���@��\@�~�@���@��\@�~�@��+@�^5@�$�@��@��h@�&�@���@���@���@��/@���@��@� �@�  @��@��P@�S�@�C�@�;d@�"�@�o@�
=@���@��H@���@���@��R@�v�@�E�@�$�@��#@���@���@��@�x�@�X@�Ĝ@��w@�dZ@�K�@�+@�
=@�@���@���@�E�@���@�O�@�G�@���@��@�j@�  @��w@�t�@�K�@�C�@�33@��@�
=@��@���@��\@�M�@��^@�?}@���@��/@���@��9@���@�j@���@���@��w@��@��@���@�l�@�C�@�"�@��y@��!@��\@�5?@�{@�@�X@��@��9@�z�@�I�@�1'@�(�@�b@��@���@��+@�E�@�@��T@���@�O�@�/@�V@��9@���@y��@i�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�"�A�$�A�&�A� �A� �A� �A��A�
=A���A���A���A���A���A���A��mA�~�A�7LAܣ�AۃA���A�oA٩�A�dZA���A�I�A�9XA���AΧ�A̸RA���A�
=Aʕ�A�XA���A�"�A�|�A�=qAÏ\A�~�A�ZA���A�bNA� �A��A�G�A�l�A���A��TA��A�VA�JA�K�A�A�A��hA�33A�bNA��jA���A��A���A�VA���A�ZA��A�oA�G�A�33A��A�=qA���A��hA�jA�
=A�7LA�^5A�C�A��A�\)A�;dA��`A��A�^5A�1'A�&�A�ffA�{A�t�A�1A���A���A��HA�hsA�x�A��wA�+A��RA��HA�r�A���A�1A��PA�n�A�hsA�Q�A~��Az�\AxI�Av��Au�#AtQ�Ar�/Ap��Anz�Ai%AfZAd�Ad�Abv�Ab^5Ab�Ab��Ab�A`ĜA\E�AZ��AX�jAV��AU�hAU&�ATbNARbNAK�mAJ�uAHr�AB1A>5?A9��A4bNA1l�A/��A/�A.��A.�A.�DA-�A)�mA'��A&(�A%��A$��A#S�A"��A"A�A!��AhsAz�A|�A�A�uA�!A�A��Ar�A-A`BA��A��A(�A\)A��AQ�AAC�A�AI�Ax�A��A�/A�A�PA
�A��A�AjA�hAVAQ�A�9AK�A   @�&�@�z�@�E�@���@��D@�A�@�(�@��m@�-@�bN@띲@�~�@�/@��m@�33@�$�@�`B@�%@���@�Z@��
@�33@��y@���@�7@���@���@���@�A�@�\)@ٲ-@ְ!@�dZ@���@�1@��@�~�@͑h@�7L@�hs@�?}@̬@���@��@���@Ȭ@��@�K�@�5?@�%@ă@�bN@�I�@��@�  @��
@���@�dZ@���@��-@�Z@�1@��P@�t�@�l�@�\)@�ȴ@�v�@�M�@��#@�@��^@���@��@��h@�+@���@��@�p�@�&�@���@�o@���@��+@��\@��\@�~�@�v�@��h@�/@��@��`@�  @�33@�E�@��h@���@���@�o@��@���@��@���@���@�ȴ@���@�@���@��`@��/@��9@�r�@�(�@���@�o@�@��T@�@���@��@�hs@�hs@�hs@�`B@�?}@�7L@��@�Ĝ@�9X@��
@���@�C�@���@�M�@�@�hs@��@���@���@�r�@�(�@��;@�K�@���@�n�@�^5@�=q@��#@�/@�(�@��P@��!@��+@�ff@��@���@��@���@��9@�Z@��@�1@�  @���@��F@���@���@��P@�o@��R@�v�@�=q@�-@��@��T@��-@��7@�p�@�X@�?}@��@���@�Ĝ@��@�A�@� �@��@��
@���@�l�@�+@�o@��@��R@���@��\@�~�@���@��\@�~�@��+@�^5@�$�@��@��h@�&�@���@���@���@��/@���@��@� �@�  @��@��P@�S�@�C�@�;d@�"�@�o@�
=@���@��H@���@���@��R@�v�@�E�@�$�@��#@���@���@��@�x�@�X@�Ĝ@��w@�dZ@�K�@�+@�
=@�@���@���@�E�@���@�O�@�G�@���@��@�j@�  @��w@�t�@�K�@�C�@�33@��@�
=@��@���@��\@�M�@��^@�?}@���@��/@���@��9@���@�j@���@���@��w@��@��@���@�l�@�C�@�"�@��y@��!@��\@�5?@�{@�@�X@��@��9@�z�@�I�@�1'@�(�@�b@��@���@��+@�E�@�@��T@���@�O�@�/@�V@��9@���@y��@i�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BM�BM�BM�BM�BM�BM�BN�BN�BO�BO�BP�BQ�BQ�BR�BQ�B_;Bm�Bp�Bu�Bv�B|�B� B�B�B��B��B�^B��B��B��B�
B�B�mB��B  BB��B�B�NB�5B�BB�B  B�jB��B��B�VB�B�=B�uB��B�1B~�Bm�BffBn�B�B��B��B�BB��B��B��BVB#�B"�B#�B%�B0!B2-B7LB;dB;dB,B\B��B�#B�qB��B�DBx�BiyB^5BVB0!B�B
=B1B
��B
ȴB
�LB
�3B
�B
��B
�hB
�7B
}�B
u�B
l�B
]/B
E�B
5?B
$�B
DB	��B	�B	�yB	�5B	��B	ŢB	�'B	�=B	w�B	n�B	jB	p�B	w�B	z�B	{�B	}�B	}�B	jB	aHB	R�B	G�B	?}B	<jB	7LB	)�B	
=B	  B��B��B��B�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�7B�B�B� B}�B{�Bx�Bw�Bw�Bv�Bv�Bu�Bt�Bt�Bs�Bs�Bs�Bv�Bs�Br�Bu�Bx�Bz�B�DB��B��B��B��B��B��B��B��B��B��B�uB�bB�oB��B��B�hB�oB�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9B�wBĜB��B�wB�}B�jB�qB�wB�qB�jB�^B�^B�qBBÖBÖBĜBƨBǮBɺBɺB��B��B��B��B��B��B��B�B�B�B��B��B��B��B�
B�B�
B�
B�
B�
B�B�B�B�)B�5B�;B�BB�HB�`B��B	1B	JB	hB	hB	{B	�B	!�B	#�B	#�B	#�B	#�B	#�B	)�B	-B	.B	.B	1'B	5?B	9XB	=qB	B�B	H�B	I�B	J�B	J�B	J�B	J�B	J�B	J�B	K�B	N�B	Q�B	Q�B	Q�B	R�B	S�B	T�B	T�B	YB	^5B	_;B	aHB	bNB	ffB	iyB	m�B	q�B	v�B	y�B	z�B	{�B	�B	�7B	�JB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�FB	�XB	�wB	�}B	�}B	ÖB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�BB	�HB	�BB	�HB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
+B
+B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
DB
PB
PB
VB
VB
VB
PB
VB
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
aB
&�B
1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BM�BM�BM�BM�BM�BM�BN�BN�BO�BO�BP�BQ�BQ�BR�BQ�B_;Bm�Bp�Bu�Bv�B|�B� B�B�B��B��B�^B��B��B��B�
B�B�mB��B  BB��B�B�NB�5B�BB�B  B�jB��B��B�VB�B�=B�uB��B�1B~�Bm�BffBn�B�B��B��B�BB��B��B��BVB#�B"�B#�B%�B0!B2-B7LB;dB;dB,B\B��B�#B�qB��B�DBx�BiyB^5BVB0!B�B
=B1B
��B
ȴB
�LB
�3B
�B
��B
�hB
�7B
}�B
u�B
l�B
]/B
E�B
5?B
$�B
DB	��B	�B	�yB	�5B	��B	ŢB	�'B	�=B	w�B	n�B	jB	p�B	w�B	z�B	{�B	}�B	}�B	jB	aHB	R�B	G�B	?}B	<jB	7LB	)�B	
=B	  B��B��B��B�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�7B�B�B� B}�B{�Bx�Bw�Bw�Bv�Bv�Bu�Bt�Bt�Bs�Bs�Bs�Bv�Bs�Br�Bu�Bx�Bz�B�DB��B��B��B��B��B��B��B��B��B��B�uB�bB�oB��B��B�hB�oB�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9B�wBĜB��B�wB�}B�jB�qB�wB�qB�jB�^B�^B�qBBÖBÖBĜBƨBǮBɺBɺB��B��B��B��B��B��B��B�B�B�B��B��B��B��B�
B�B�
B�
B�
B�
B�B�B�B�)B�5B�;B�BB�HB�`B��B	1B	JB	hB	hB	{B	�B	!�B	#�B	#�B	#�B	#�B	#�B	)�B	-B	.B	.B	1'B	5?B	9XB	=qB	B�B	H�B	I�B	J�B	J�B	J�B	J�B	J�B	J�B	K�B	N�B	Q�B	Q�B	Q�B	R�B	S�B	T�B	T�B	YB	^5B	_;B	aHB	bNB	ffB	iyB	m�B	q�B	v�B	y�B	z�B	{�B	�B	�7B	�JB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�FB	�XB	�wB	�}B	�}B	ÖB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�BB	�HB	�BB	�HB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
+B
+B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
DB
PB
PB
VB
VB
VB
PB
VB
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
aB
&�B
1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140821                              AO  ARCAADJP                                                                    20181024140821    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140821  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140821  QCF$                G�O�G�O�G�O�0               