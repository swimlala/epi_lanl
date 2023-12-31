CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:49Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191749  20181005191749  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�co�1   @��e`�@5Y������dz��vȴ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C��C��C�  C�  C��C��C��3C��3C�  C�  C��3C��fC�  C��C��C��C�  C�  C�  C�  C�  C��3C��3C��C��C�  C�  C��C�  C��3C�  C��C�  C��3C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C��C�  C��3C�  C��C��C��3C��C�  C��3C��C�  C�  C��C��C��3C�  C��3C�  C�  C��3C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D � D  D�fDfD�fDfD� D  Dy�D  D� D  D� D  D� D��D� D	�D	� D
fD
� DfD� D  Dy�D  D� DfD�fDfD� D  Dy�DfDy�D��D� DfDy�DfD� D  D�fD  D� DfD� DfDy�D  D�fD  D�fDfD� D��D�fD  D� D  D� D  D� D   D y�D!fD!�fD"fD"y�D#  D#y�D$  D$� D%  D%� D%��D&�fD&��D'y�D'��D(y�D)fD)�fD)��D*s3D*��D+�fD+��D,� D-fD-y�D.  D.y�D/  D/y�D/��D0� D0��D1s3D2  D2� D3  D3� D4  D4�fD5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?y�D@  D@y�DAfDA�fDB  DB� DC  DC� DD  DD�fDE  DE�fDF  DFy�DG  DG� DH  DH� DI  DI� DJfDJ�fDKfDK��DL  DL�fDM  DM� DN  DNy�DO  DO� DP  DPy�DQ  DQ� DRfDR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[y�D\fD\� D]fD]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Db��Dcy�DdfDd� De  De� DffDf�fDf��Dg� Dh  Dh� Di  Di�fDjfDj� Dj��Dk�fDl  Dl� Dm  Dm�fDnfDn�fDo  Doy�Dp  Dp�fDp��Dq� Dr  Dr�fDs  Ds�fDt  Dt�fDufDu�fDv  Dv� Dw  Dw� Dw��Dy�
D�8 D�Ǯ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@˅AA%AD(�AeA��HA��HA��HA��HA��HA��HA��HA��HBp�B	p�Bp�Bp�B!p�B)p�B1p�B9p�BAp�BIp�BQp�BYp�Bap�Bip�Bqp�Byp�B��RB��RB��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBĸRBȸRB̸RB��BԸRBظRBܸRB�RB�RB�RB�RB�RB��RB��RB��RC \)C\)C\)C\)C\)C
\)C\)C\)C\)C\)C\)C\)C\)C\)C\)Cu�C \)C"\)C$\)C&\)C(\)C*\)C,\)C.\)C0\)C2\)C4\)C6u�C8u�C:u�C<u�C>\)C@\)CB\)CD\)CF\)CH\)CJ\)CL\)CN\)CP\)CR\)CT\)CV\)CX\)CZ\)C\\)C^\)C`B�Cb\)Cd\)Cf\)Ch\)Cj\)Cl\)Cn\)Cp\)Cr\)Ct\)Cv\)Cx\)Cz\)C|u�C~\)C�.C�.C�.C�.C�.C�.C�.C�.C�!GC�!GC�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�!GC�!GC�!GC�!GC�.C�:�C�:�C�:�C�.C�.C�:�C�:�C�!GC�!GC�.C�.C�!GC�zC�.C�:�C�:�C�:�C�.C�.C�.C�.C�.C�!GC�!GC�:�C�:�C�.C�.C�:�C�.C�!GC�.C�:�C�.C�!GC�.C�:�C�:�C�.C�!GC�.C�.C�.C�.C�.C�.C�.C�:�C�:�C�.C�!GC�:�C�.C�.C�.C�!GC�.C�.C�.C�.C�!GC�!GC�.C�.C�.C�:�C�:�C�.C�!GC�.C�:�C�:�C�!GC�:�C�.C�!GC�:�C�.C�.C�:�C�:�C�!GC�.C�!GC�.C�.C�!GC�.C�:�C�.C�:�C�.C�.C�.C�.C�.C�.C�.C�.C�.C�.C�!GD �D �
D
D�pDpD�pDpD�
D
D��D
D�
D
D�
D
D�
D�D�
D	#�D	�
D
pD
�
DpD�
D
D��D
D�
DpD�pDpD�
D
D��DpD��D�D�
DpD��DpD�
D
D�pD
D�
DpD�
DpD��D
D�pD
D�pDpD�
D�D�pD
D�
D
D�
D
D�
D 
D ��D!pD!�pD"pD"��D#
D#��D$
D$�
D%
D%�
D&�D&�pD'�D'��D(�D(��D)pD)�pD*�D*�=D+�D+�pD,�D,�
D-pD-��D.
D.��D/
D/��D0�D0�
D1�D1�=D2
D2�
D3
D3�
D4
D4�pD5
D5�pD6
D6�
D7
D7�
D8
D8�
D9
D9�
D:�D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>��D?
D?��D@
D@��DApDA�pDB
DB�
DC
DC�
DD
DD�pDE
DE�pDF
DF��DG
DG�
DH
DH�
DI
DI�
DJpDJ�pDKpDK��DL
DL�pDM
DM�
DN
DN��DO
DO�
DP
DP��DQ
DQ�
DRpDR�
DS
DS�
DT
DT��DU
DU�
DV
DV�
DW
DW�
DX
DX�
DY�DY�
DZ
DZ�
D[
D[��D\pD\�
D]pD]�
D^
D^��D_
D_�
D`
D`�
Da
Da�
Db
Db�
Dc�Dc��DdpDd�
De
De�
DfpDf�pDg�Dg�
Dh
Dh�
Di
Di�pDjpDj�
Dk�Dk�pDl
Dl�
Dm
Dm�pDnpDn�pDo
Do��Dp
Dp�pDq�Dq�
Dr
Dr�pDs
Ds�pDt
Dt�pDupDu�pDv
Dv�
Dw
Dw�
Dx�Dy�D�C�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�+A�-A�-A�-A�(�A�+A�$�A�&�A�&�A�(�A�+A� �A�JA��`A���A���A��jA��jA���A��\A�ZA�  A�ĜA��A�bNA�/A��-A��A�p�A�ffA�O�A�?}A�/A���A��RA���A�t�A�O�A�;dA��A�JA��mA�v�A��RA�?}A�ffA�-A��!A�I�A�ZA��A�dZA���A���A�I�A�VA�9XA���A��yA��#A���A���A���A���A�v�A�S�A�E�A�(�A��mA�M�A�dZA�ffA��#A���A�r�A���A��yA�K�A���A�ƨA�9XA�x�A�?}A�oA���A��A��A��A��A�ĜA���A��PA�XA�{A�+A��
A��A��\A���A}�;Az��Ay
=AuAr�!Ao��Am��AlffAk33Af��Ad=qAcoAa33A`JA_S�A^A�A[l�AY�AX�AW�FAV1'ATZAS
=AQt�AMAM&�ALAK"�AJ$�AIXAH��AG��AF��AF�jAFv�AE\)ADVAC�mAC�AB�+AA�#A@JA>1'A>bA=��A="�A<�DA;�A:9XA9��A9XA7��A5��A4(�A3`BA2��A2��A2=qA1|�A0��A0��A/�A.��A.VA-�A,n�A*�\A)��A($�A&9XA%��A%�mA%ƨA%��A%�A$��A#�A#��A"��A!��A ��A ��A 1At�A�A~�A��A(�A�#A��A;dA
=A�!A1A�A�AȴA�A�A�A+A1AXA~�AbA|�A�yAĜA�#A�^A?}A��An�A��Al�A
�A
n�A	��A	K�A�AE�AQ�A1A �+A   @�l�@���@�=q@�@�`B@���@�^5@�/@��
@��^@�7@��@�@�dZ@�n�@�7L@�C�@��@��/@���@�E�@��@���@�"�@ޏ\@ܣ�@ڗ�@�5?@��@�@��#@��/@ְ!@���@��@� �@ӝ�@���@��m@���@���@�r�@��H@�p�@��@�j@�Q�@�b@��H@���@�"�@\@�M�@��7@�z�@��@���@��`@�1@��y@�-@���@��j@�K�@�ȴ@��\@��@��9@�ƨ@��H@���@�E�@�hs@�bN@�I�@�1'@��
@��@��H@�ȴ@�E�@��7@�p�@���@�~�@�~�@�=q@��T@��T@�G�@�r�@��@��/@��u@�t�@�S�@��\@���@��7@��@��@��/@�Ĝ@�V@�Ĝ@��@��@�\)@�;d@��@��H@���@��R@��R@�ȴ@��R@�v�@�ff@�M�@�=q@�J@���@�7L@��@���@��@��`@��9@��D@��@���@�S�@���@�$�@���@��@���@��u@��m@��@�l�@�
=@�v�@�@�@��-@��@�@���@��@�V@��F@��w@��@�|�@�dZ@��P@�t�@�C�@��@��@���@���@��@�-@�5?@��-@�V@��u@�Z@�9X@�(�@� �@��@�b@�1@�b@��@��w@��@�\)@�+@��@���@��\@�M�@�5?@�{@��T@��-@���@�x�@�G�@��@��9@�Z@�9X@�b@���@��m@��;@���@�|�@�S�@��@��@�n�@�$�@���@���@�G�@��9@��@��@���@��@�bN@�Q�@�9X@��;@�ƨ@��F@��@���@�l�@�S�@��@��@�ȴ@���@�v�@�-@���@��7@�`B@���@���@���@��D@�j@�Z@�b@���@�ƨ@��P@�C�@�"�@��@��@���@�^5@�{@��^@��h@�p�@�hs@�`B@�7L@��@���@���@��9@��D@�j@� �@��
@��P@�33@���@��H@�($@{]�@i}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+A�+A�-A�-A�-A�(�A�+A�$�A�&�A�&�A�(�A�+A� �A�JA��`A���A���A��jA��jA���A��\A�ZA�  A�ĜA��A�bNA�/A��-A��A�p�A�ffA�O�A�?}A�/A���A��RA���A�t�A�O�A�;dA��A�JA��mA�v�A��RA�?}A�ffA�-A��!A�I�A�ZA��A�dZA���A���A�I�A�VA�9XA���A��yA��#A���A���A���A���A�v�A�S�A�E�A�(�A��mA�M�A�dZA�ffA��#A���A�r�A���A��yA�K�A���A�ƨA�9XA�x�A�?}A�oA���A��A��A��A��A�ĜA���A��PA�XA�{A�+A��
A��A��\A���A}�;Az��Ay
=AuAr�!Ao��Am��AlffAk33Af��Ad=qAcoAa33A`JA_S�A^A�A[l�AY�AX�AW�FAV1'ATZAS
=AQt�AMAM&�ALAK"�AJ$�AIXAH��AG��AF��AF�jAFv�AE\)ADVAC�mAC�AB�+AA�#A@JA>1'A>bA=��A="�A<�DA;�A:9XA9��A9XA7��A5��A4(�A3`BA2��A2��A2=qA1|�A0��A0��A/�A.��A.VA-�A,n�A*�\A)��A($�A&9XA%��A%�mA%ƨA%��A%�A$��A#�A#��A"��A!��A ��A ��A 1At�A�A~�A��A(�A�#A��A;dA
=A�!A1A�A�AȴA�A�A�A+A1AXA~�AbA|�A�yAĜA�#A�^A?}A��An�A��Al�A
�A
n�A	��A	K�A�AE�AQ�A1A �+A   @�l�@���@�=q@�@�`B@���@�^5@�/@��
@��^@�7@��@�@�dZ@�n�@�7L@�C�@��@��/@���@�E�@��@���@�"�@ޏ\@ܣ�@ڗ�@�5?@��@�@��#@��/@ְ!@���@��@� �@ӝ�@���@��m@���@���@�r�@��H@�p�@��@�j@�Q�@�b@��H@���@�"�@\@�M�@��7@�z�@��@���@��`@�1@��y@�-@���@��j@�K�@�ȴ@��\@��@��9@�ƨ@��H@���@�E�@�hs@�bN@�I�@�1'@��
@��@��H@�ȴ@�E�@��7@�p�@���@�~�@�~�@�=q@��T@��T@�G�@�r�@��@��/@��u@�t�@�S�@��\@���@��7@��@��@��/@�Ĝ@�V@�Ĝ@��@��@�\)@�;d@��@��H@���@��R@��R@�ȴ@��R@�v�@�ff@�M�@�=q@�J@���@�7L@��@���@��@��`@��9@��D@��@���@�S�@���@�$�@���@��@���@��u@��m@��@�l�@�
=@�v�@�@�@��-@��@�@���@��@�V@��F@��w@��@�|�@�dZ@��P@�t�@�C�@��@��@���@���@��@�-@�5?@��-@�V@��u@�Z@�9X@�(�@� �@��@�b@�1@�b@��@��w@��@�\)@�+@��@���@��\@�M�@�5?@�{@��T@��-@���@�x�@�G�@��@��9@�Z@�9X@�b@���@��m@��;@���@�|�@�S�@��@��@�n�@�$�@���@���@�G�@��9@��@��@���@��@�bN@�Q�@�9X@��;@�ƨ@��F@��@���@�l�@�S�@��@��@�ȴ@���@�v�@�-@���@��7@�`B@���@���@���@��D@�j@�Z@�b@���@�ƨ@��P@�C�@�"�@��@��@���@�^5@�{@��^@��h@�p�@�hs@�`B@�7L@��@���@���@��9@��D@�j@� �@��
@��P@�33@���@��H@�($@{]�@i}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B  B  B  B  BBBBBBBBBBB  B  B  BBB%BDB�B/B<jBK�BQ�B\)Bt�B�B�7B�DB�oB��B��B�B�-B�'B�'B�-B�3B�LB�XB�^B�XB�-B�B�qBɺB��B��BB�dB�dB�qB�dB�XB�9B��B�uB�PB�B� B� BjB`BB_;B^5B]/B\)BYBS�BM�BF�B7LB�BPBB��B�yB�)BÖB��B�7Bv�BW
B>wB-B"�B�B
��B
�NB
��B
��B
��B
ǮB
�XB
��B
��B
�B
hsB
N�B
6FB
&�B

=B	��B	�`B	�B	��B	��B	��B	��B	�VB	�B	z�B	u�B	n�B	bNB	YB	VB	R�B	XB	R�B	L�B	F�B	;dB	8RB	2-B	.B	&�B	"�B	$�B	"�B	�B	�B	�B	�B	�B	�B	{B	\B	
=B	  B��B��B�B�B�B�`B�TB�BB�)B�BɺBB��B�}B�qB�dB�XB�RB�FB�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�bB�\B�VB�PB�JB�DB�=B�=B�=B�7B�1B�%B�B�B�B�B�B�B� B}�B}�B|�B{�Bz�Bx�Bx�Bw�Bv�Bv�Bu�Bu�Bt�Bt�Bs�Bq�Bp�Bn�Bl�BhsBdZBdZBjBn�Bp�Bq�Bq�Bq�Bp�Bs�Br�Bq�Bp�Bo�Bn�BjBhsBe`BcTBbNBaHB`BB`BB_;B_;B`BB`BB`BB_;BaHBe`BffBffBe`Be`BhsBl�Bl�Bm�Bm�Bl�Bl�Bm�Bn�Bq�Br�Bv�B{�B}�B~�B�B�B�B�B�B�B�B�B�B�B�1B�7B�DB�DB�JB�JB�hB��B��B��B��B��B��B��B��B�B�B�-B�-B�3B�FB�RB�qB�qB��BɺB��B�B�NB�yB�B�B�B�B�B��B	B	B	B	%B	1B	%B	%B	+B	1B		7B	DB	\B	bB	bB	oB	�B	�B	�B	�B	#�B	&�B	'�B	,B	0!B	33B	5?B	8RB	9XB	:^B	>wB	F�B	K�B	L�B	M�B	O�B	O�B	P�B	S�B	W
B	[#B	aHB	dZB	gmB	hsB	m�B	n�B	o�B	o�B	o�B	p�B	q�B	r�B	u�B	x�B	z�B	|�B	|�B	|�B	|�B	� B	�B	�=B	�JB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�RB	�^B	�dB	�jB	�qB	��B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�`B	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 B
�B
)_222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B  B  B  B  BBBBBBBBBBB  B  B  BBB%BDB�B/B<jBK�BQ�B\)Bt�B�B�7B�DB�oB��B��B�B�-B�'B�'B�-B�3B�LB�XB�^B�XB�-B�B�qBɺB��B��BB�dB�dB�qB�dB�XB�9B��B�uB�PB�B� B� BjB`BB_;B^5B]/B\)BYBS�BM�BF�B7LB�BPBB��B�yB�)BÖB��B�7Bv�BW
B>wB-B"�B�B
��B
�NB
��B
��B
��B
ǮB
�XB
��B
��B
�B
hsB
N�B
6FB
&�B

=B	��B	�`B	�B	��B	��B	��B	��B	�VB	�B	z�B	u�B	n�B	bNB	YB	VB	R�B	XB	R�B	L�B	F�B	;dB	8RB	2-B	.B	&�B	"�B	$�B	"�B	�B	�B	�B	�B	�B	�B	{B	\B	
=B	  B��B��B�B�B�B�`B�TB�BB�)B�BɺBB��B�}B�qB�dB�XB�RB�FB�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�bB�\B�VB�PB�JB�DB�=B�=B�=B�7B�1B�%B�B�B�B�B�B�B� B}�B}�B|�B{�Bz�Bx�Bx�Bw�Bv�Bv�Bu�Bu�Bt�Bt�Bs�Bq�Bp�Bn�Bl�BhsBdZBdZBjBn�Bp�Bq�Bq�Bq�Bp�Bs�Br�Bq�Bp�Bo�Bn�BjBhsBe`BcTBbNBaHB`BB`BB_;B_;B`BB`BB`BB_;BaHBe`BffBffBe`Be`BhsBl�Bl�Bm�Bm�Bl�Bl�Bm�Bn�Bq�Br�Bv�B{�B}�B~�B�B�B�B�B�B�B�B�B�B�B�1B�7B�DB�DB�JB�JB�hB��B��B��B��B��B��B��B��B�B�B�-B�-B�3B�FB�RB�qB�qB��BɺB��B�B�NB�yB�B�B�B�B�B��B	B	B	B	%B	1B	%B	%B	+B	1B		7B	DB	\B	bB	bB	oB	�B	�B	�B	�B	#�B	&�B	'�B	,B	0!B	33B	5?B	8RB	9XB	:^B	>wB	F�B	K�B	L�B	M�B	O�B	O�B	P�B	S�B	W
B	[#B	aHB	dZB	gmB	hsB	m�B	n�B	o�B	o�B	o�B	p�B	q�B	r�B	u�B	x�B	z�B	|�B	|�B	|�B	|�B	� B	�B	�=B	�JB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�RB	�^B	�dB	�jB	�qB	��B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�`B	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 B
�B
)_222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.36 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191749                              AO  ARCAADJP                                                                    20181005191749    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191749  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191749  QCF$                G�O�G�O�G�O�8000            