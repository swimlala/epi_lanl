CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:17Z creation      
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
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        `   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        j0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        rP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        |x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140817  20181024140817  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               HA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @����Pye1   @���m�G�@2*=p��
�c���$�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      HB   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  C   C  C  C  C  C
�C  C  C  C  C  C�C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@�CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D fD � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fDfD� D  D� DfD� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��Dy�D  D� D  D� D  D� D��D� D  D� D   D � D ��D!y�D"  D"� D  D� D��Dy�D��Dy�D  D� D  D� D  D� D��D� D  D� D   D � D ��D!y�D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D;��D<y�D=  D=� D>fD>� D?  D?� D@  D@� DA  DAy�DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DOy�DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^y�D^��D_y�D_��D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd�fDe  De� DffDf�fDg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy�{D�7�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A��\A�\)B �B�BG�B�B �B(�B0�B8�B@�BH�BP�BXG�B`�Bh�Bp�Bx�B��=B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�=B��=B�W
C +�C+�C+�C+�C+�C
EC+�C+�C+�C+�C+�CEC+�C+�C+�C�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6EC8+�C:+�C<+�C>+�C@ECB+�CD+�CFECH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~EC�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C�"�C�"�C�"�C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C�"�D GD ��D
�D��D
�D��D
�D��DGD��D
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
��D
�D�GDGD��D
�D��DGD��D
�D��D{D�{D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D{D�{D{D�{D
�D��D
�D��D
�D��D{D��D
�D��D 
�D ��D!{D!�{D"
�D"��D
�D��D{D�{D{D�{D
�D��D
�D��D
�D��D{D��D
�D��D 
�D ��D!{D!�{D"
�D"��D#
�D#��D$
�D$��D%GD%��D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3��D4
�D4��D5
�D5��D6
�D6��D7
�D7�GD8
�D8��D9
�D9��D:
�D:��D;
�D;��D<{D<�{D=
�D=��D>GD>��D?
�D?��D@
�D@��DA
�DA�{DB
�DB�GDC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG�GDH
�DH��DI
�DI��DJGDJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO{DO�{DP
�DP��DQ
�DQ��DR
�DR�{DS
�DS��DT
�DT�{DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^{D^�{D_{D_�{D`{D`��Da
�Da��Db
�Db�GDc
�Dc��Dd
�Dd�GDe
�De��DfGDf�GDg
�Dg�{Dh
�Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt��Du
�Du��Dv
�Dv��Dw
�Dw��Dw�Dy�\D�=D�>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�n�A�p�A�p�A�p�A�n�A�ffA�XA�Aޝ�A�n�A�dZA�\)A�S�A�O�A�G�A�;dA�/A�"�A��A�1A��A��/Aݺ^A�^5A�/A���A�oA���A��HA��A�G�A�%A�1A�A��A�5?Aϲ-A�/A̧�A�  Aʣ�A��
A���A�33A�{Aǣ�A�K�A���A�bA�{A� �A�I�A���A��A�`BA�v�A� �A�hsA���A�jA�/A��`A���A�G�A��#A�$�A�5?A�ƨA�n�A�VA��A�|�A�&�A��A� �A�^5A�M�A��A���A��/A�jA��PA�5?A�/A�$�A��TA�p�A���A���A�
=A�=qA�1A�
=A��A���A�ĜA�$�A�v�A�VA��!A��`A���A�/A��`A�G�A��!A���A�G�A��RA�33A�bNA}�PAz��AxffAs��Arr�Ap  Akl�Ai�TAfv�Ab�HA`-A\��AY�AT^5AQ�#AO�mAN$�AM33ALI�AK�AKp�AHv�AE��AE�AC�
AC?}AB�RAA�A?�PA>�uA=�A<M�A:A8�A6��A5�wA4�9A3�A0ĜA//A-��A+��A+/A)�mA)hsA(��A(M�A'/A&=qA%\)A#��A!��A!��A �`A�7A&�A��A��AE�AK�AVAI�A�^A�9A?}A�!A9XAK�Ar�A��AVA�;A�^A��AVA��A�AdZAp�A��At�A7LA�AĜA��A��A�RAƨAhsA33A ��A r�A ^5A �@��y@�l�@�\)@��m@�9X@���@���@�C�@��j@��@�G�@���@�`B@��^@�\)@�R@�h@��@�r�@�u@���@�h@��@�9X@�n�@�@�@�(�@��
@�33@�&�@�A�@㝲@�33@�{@�b@�+@�M�@�?}@�(�@ۥ�@�"�@�~�@��@�X@؛�@ׅ@պ^@�bN@Ӯ@�ȴ@�@ёh@�hs@��@��@Ѓ@���@Ϯ@�t�@�M�@̛�@�@ȴ9@�"�@���@�~�@�5?@��@��;@�;d@���@�=q@��#@��7@�Z@�;d@�~�@�@�p�@���@�b@�S�@�o@�-@�hs@�1'@��m@��y@���@��@�&�@��u@�bN@�b@�1@�1'@�j@�z�@��m@��!@�@���@�x�@�X@��/@�1'@��
@��@�K�@��H@�M�@��@��@�@�hs@�?}@�&�@�1'@�j@�z�@��m@��!@�@���@�x�@�X@��/@�1'@��
@��@�K�@��H@�M�@��@��@�@�hs@�?}@�&�@���@���@��9@�r�@�S�@�+@�o@���@���@�n�@���@�x�@�V@�Z@���@�|�@�C�@��!@�/@��9@�z�@�Q�@�b@���@��
@�dZ@�+@�o@��@���@���@��7@�G�@��@�%@��7@��h@�X@�V@���@�I�@�1'@���@��w@��@�
=@�~�@��7@�M�@��\@���@��`@��D@�bN@�l�@��@�;d@��+@�ff@���@�ȴ@�x�@�V@��#@���@�7L@��@�1'@��;@�ƨ@���@�K�@��y@�n�@�E�@��T@�p�@���@���@�j@���@�S�@�@��@�n�@�J@�`B@�/@�V@��@��D@�bN@�Z@�Q�@�I�@�  @�l�@��H@��+@�ff@�M�@�-@�@�J@��@��-@��@�?}@�&�@��@��@��@��@��`@�bN@� �@��F@�"�@��!@�M�@��@�@���@�`B@��@��/@��j@��9@���@��@��j@���@��D@�z�@�9X@�1@��@��;@��;@��
@��F@���@�;d@�o@��H@��+@�E�@�J@���@�V@���@��D@�r�@�Q�@��@��
@�+@���@��@�x�@�x�@�7L@���@���@�bN@�b@~��@~ȴ@~��@~V@}�-@{��@h]d@[4�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�n�A�n�A�p�A�p�A�p�A�n�A�ffA�XA�Aޝ�A�n�A�dZA�\)A�S�A�O�A�G�A�;dA�/A�"�A��A�1A��A��/Aݺ^A�^5A�/A���A�oA���A��HA��A�G�A�%A�1A�A��A�5?Aϲ-A�/A̧�A�  Aʣ�A��
A���A�33A�{Aǣ�A�K�A���A�bA�{A� �A�I�A���A��A�`BA�v�A� �A�hsA���A�jA�/A��`A���A�G�A��#A�$�A�5?A�ƨA�n�A�VA��A�|�A�&�A��A� �A�^5A�M�A��A���A��/A�jA��PA�5?A�/A�$�A��TA�p�A���A���A�
=A�=qA�1A�
=A��A���A�ĜA�$�A�v�A�VA��!A��`A���A�/A��`A�G�A��!A���A�G�A��RA�33A�bNA}�PAz��AxffAs��Arr�Ap  Akl�Ai�TAfv�Ab�HA`-A\��AY�AT^5AQ�#AO�mAN$�AM33ALI�AK�AKp�AHv�AE��AE�AC�
AC?}AB�RAA�A?�PA>�uA=�A<M�A:A8�A6��A5�wA4�9A3�A0ĜA//A-��A+��A+/A)�mA)hsA(��A(M�A'/A&=qA%\)A#��A!��A!��A �`A�7A&�A��A��AE�AK�AVAI�A�^A�9A?}A�!A9XAK�Ar�A��AVA�;A�^A��AVA��A�AdZAp�A��At�A7LA�AĜA��A��A�RAƨAhsA33A ��A r�A ^5A �@��y@�l�@�\)@��m@�9X@���@���@�C�@��j@��@�G�@���@�`B@��^@�\)@�R@�h@��@�r�@�u@���@�h@��@�9X@�n�@�@�@�(�@��
@�33@�&�@�A�@㝲@�33@�{@�b@�+@�M�@�?}@�(�@ۥ�@�"�@�~�@��@�X@؛�@ׅ@պ^@�bN@Ӯ@�ȴ@�@ёh@�hs@��@��@Ѓ@���@Ϯ@�t�@�M�@̛�@�@ȴ9@�"�@���@�~�@�5?@��@��;@�;d@���@�=q@��#@��7@�Z@�;d@�~�@�@�p�@���@�b@�S�@�o@�-@�hs@�1'@��m@��y@���@��@�&�@��u@�bN@�b@�1@�1'@�j@�z�@��m@��!@�@���@�x�@�X@��/@�1'@��
@��@�K�@��H@�M�@��@��@�@�hs@�?}@�&�@�1'@�j@�z�@��m@��!@�@���@�x�@�X@��/@�1'@��
@��@�K�@��H@�M�@��@��@�@�hs@�?}@�&�@���@���@��9@�r�@�S�@�+@�o@���@���@�n�@���@�x�@�V@�Z@���@�|�@�C�@��!@�/@��9@�z�@�Q�@�b@���@��
@�dZ@�+@�o@��@���@���@��7@�G�@��@�%@��7@��h@�X@�V@���@�I�@�1'@���@��w@��@�
=@�~�@��7@�M�@��\@���@��`@��D@�bN@�l�@��@�;d@��+@�ff@���@�ȴ@�x�@�V@��#@���@�7L@��@�1'@��;@�ƨ@���@�K�@��y@�n�@�E�@��T@�p�@���@���@�j@���@�S�@�@��@�n�@�J@�`B@�/@�V@��@��D@�bN@�Z@�Q�@�I�@�  @�l�@��H@��+@�ff@�M�@�-@�@�J@��@��-@��@�?}@�&�@��@��@��@��@��`@�bN@� �@��F@�"�@��!@�M�@��@�@���@�`B@��@��/@��j@��9@���@��@��j@���@��D@�z�@�9X@�1@��@��;@��;@��
@��F@���@�;d@�o@��H@��+@�E�@�J@���@�V@���@��D@�r�@�Q�@��@��
@�+@���@��@�x�@�x�@�7L@���@���@�bN@�b@~��@~ȴ@~��@~V@}�-@{��@h]d@[4�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�
B
�
B
�
B
�
B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�#B
�HBB#�B5?B?}BT�Bo�B�=B��B��B�B�9B�}BÖB��B��B�/B�;B�B1B�B�B)�B49B:^BS�Bn�Bt�B�B�JB��B�^B��B�
BÖB�^B�XB�XB�XB�RB�FB�?B�FB�dB�^B�RB�9B�B��B��B�uBw�Bm�BaHBN�BC�B49B-B!�B�B�B�B�B\B	7BB��B�B�)BɺB��B�JB}�Bq�BcTBK�B5?B$�B�B  B
�B
��B
�bB
�B
x�B
o�B
ffB
W
B
2-B
�B
1B	�B	�NB	��B	�XB	�B	��B	�B	jB	L�B	5?B	�B	
=B	%B	B	  B��B��B��B�B�sB�fB�HB�5B�#B�B��B��B��BǮB��B�LB�!B��B��B��B��B��B��B�{B�oB�\B�VB�VB�PB�1B�B�B{�By�Bz�Bz�Bv�Bw�By�B}�B~�B}�B{�B�JB��B��B��B��B��B�B�'B�'B�-B�3B�FB�LB�-B��B��B��B��B�B�B��B�PBu�BdZBO�BK�BL�BM�BM�BM�BO�BS�BVBT�BXBbNBm�Br�Be`BL�BH�BYB_;BdZBx�B�bB��B�bB�hB��B��B��B��B��B��B�{B��B�uB��B�B�!B�B�!B�9B�9B�9B�9B�?B�LB�RB�XB�^B�dB�jB�qB�wB�wB��B��BĜBȴB��B��B��B��B��B��B��B��B��B�
B�
B�
B�B�5B�mB�B�B��B��B��B	B	1B	PB	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	&�B	&�B	)�B	/B	33B	33B	33B	6FB	9XB	:^B	>wB	?}B	A�B	C�B	D�B	G�B	I�B	H�B	H�B	I�B	J�B	L�B	N�B	S�B	T�B	W
B	[#B	_;B	aHB	bNB	cTB	hsB	m�B��B	A�B	C�B	D�B	G�B	I�B	H�B	H�B	I�B	J�B	L�B	N�B	S�B	T�B	W
B	[#B	_;B	aHB	bNB	cTB	hsB	m�B	o�B	p�B	q�B	r�B	s�B	x�B	y�B	z�B	z�B	{�B	|�B	~�B	~�B	� B	�B	�B	�B	�+B	�=B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�FB	�FB	�LB	�RB	�jB	��B	��B	��B	��B	��B	��B	B	ŢB	ȴB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�NB	�BB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
%B
%B
+B
1B
	7B

=B

=B
DB
JB
PB
VB
\B
bB
hB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
#�B
#�B
$�B
%,B
2�B
@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�
B
�
B
�
B
�
B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�#B
�HBB#�B5?B?}BT�Bo�B�=B��B��B�B�9B�}BÖB��B��B�/B�;B�B1B�B�B)�B49B:^BS�Bn�Bt�B�B�JB��B�^B��B�
BÖB�^B�XB�XB�XB�RB�FB�?B�FB�dB�^B�RB�9B�B��B��B�uBw�Bm�BaHBN�BC�B49B-B!�B�B�B�B�B\B	7BB��B�B�)BɺB��B�JB}�Bq�BcTBK�B5?B$�B�B  B
�B
��B
�bB
�B
x�B
o�B
ffB
W
B
2-B
�B
1B	�B	�NB	��B	�XB	�B	��B	�B	jB	L�B	5?B	�B	
=B	%B	B	  B��B��B��B�B�sB�fB�HB�5B�#B�B��B��B��BǮB��B�LB�!B��B��B��B��B��B��B�{B�oB�\B�VB�VB�PB�1B�B�B{�By�Bz�Bz�Bv�Bw�By�B}�B~�B}�B{�B�JB��B��B��B��B��B�B�'B�'B�-B�3B�FB�LB�-B��B��B��B��B�B�B��B�PBu�BdZBO�BK�BL�BM�BM�BM�BO�BS�BVBT�BXBbNBm�Br�Be`BL�BH�BYB_;BdZBx�B�bB��B�bB�hB��B��B��B��B��B��B�{B��B�uB��B�B�!B�B�!B�9B�9B�9B�9B�?B�LB�RB�XB�^B�dB�jB�qB�wB�wB��B��BĜBȴB��B��B��B��B��B��B��B��B��B�
B�
B�
B�B�5B�mB�B�B��B��B��B	B	1B	PB	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	&�B	&�B	)�B	/B	33B	33B	33B	6FB	9XB	:^B	>wB	?}B	A�B	C�B	D�B	G�B	I�B	H�B	H�B	I�B	J�B	L�B	N�B	S�B	T�B	W
B	[#B	_;B	aHB	bNB	cTB	hsB	m�B��B	A�B	C�B	D�B	G�B	I�B	H�B	H�B	I�B	J�B	L�B	N�B	S�B	T�B	W
B	[#B	_;B	aHB	bNB	cTB	hsB	m�B	o�B	p�B	q�B	r�B	s�B	x�B	y�B	z�B	z�B	{�B	|�B	~�B	~�B	� B	�B	�B	�B	�+B	�=B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�FB	�FB	�LB	�RB	�jB	��B	��B	��B	��B	��B	��B	B	ŢB	ȴB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�NB	�BB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
%B
%B
+B
1B
	7B

=B

=B
DB
JB
PB
VB
\B
bB
hB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
#�B
#�B
$�B
%,B
2�B
@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140817                              AO  ARCAADJP                                                                    20181024140817    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140817  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140817  QCF$                G�O�G�O�G�O�4000            