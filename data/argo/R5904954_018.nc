CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:52Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191652  20181005191652  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׫����>1   @׫�M���@40bM���c�^5?|�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>�C@  CB�CD�CF�CH�CJ  CL  CN�CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C��3C�  C�  C��C��C��C��C��C��C��C��C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C��C��C��C��C�  C��3C��C��C��3C��3C��3C��3C�  C�  C�  C��C�  C��C�  C�  C��C�  C�  C��3C��C��C�  C��C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C��3C��3C��fC��fC�  C��C��3C�  C��C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C��C�  C��C��C�  C�  C��3D � D  D�fD  D� D�D�fD�D�fD��Dy�D��Dy�D��D�fDfD� D��D	� D
fD
y�DfD� D��D� D  D� D  D� D  D� D  D�fDfD�fD  Dy�D  D� D�D�fD  D� D��Dy�D  D� D��Dy�D��Dy�D�3Dy�D  D� DfD� D  D�fD  D� D  D�fD��D y�D!  D!� D"  D"y�D#  D#� D$  D$�fD%  D%�fD&  D&� D'  D'� D(  D(� D)  D)�fD*  D*y�D+  D+� D+��D,� D-fD-� D.  D.�fD/  D/y�D/��D0� D1fD1�fD2  D2y�D2��D3�fD4  D4y�D5  D5�fD5��D6y�D7  D7�fD8  D8y�D9  D9�fD:  D:�fD;fD;� D<  D<�fD<��D=� D>fD>�fD?fD?��D@fD@� DAfDA�fDB  DB� DB��DC� DD  DD�fDE�DE�fDFfDFy�DF��DGy�DG��DHy�DI  DI�fDI��DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS�fDT  DTy�DU  DU� DV  DV�fDW  DW� DX�DX�fDYfDY� DY��DZ�fD[fD[�fD\  D\y�D]  D]� D^fD^� D^��D_y�D`  D`� Da  Da� Da��Dby�DcfDc�fDd  Dd� DefDe� De��Df� Dg  Dgs3Dg��Dhy�Dh��Diy�Di��Dj� DkfDk� Dk��Dls3Dl��Dm�fDn  Dn� Do  Doy�Dp  Dp�fDqfDq� Dq��Dry�Dr��Ds� DtfDt�fDufDu� Dv  Dv� Dw  DwffDy^�D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @'
=@��@ÅAA!AAAaA��HA��HA��HA�{A��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BX�
B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�B�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$�C&)C()C*)C,)C.)C0)C2)C4)C6�C8�C:)C<)C>5�C@)CB5�CD5�CF5�CH5�CJ)CL)CN5�CP)CR)CT)CV)CX�CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl�Cn�Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�GC�GC�C�C��C��C��C��C��C��C��C��C�C�C�C�C�GC�C�C��C�C�C�C�GC�C�C�C�C�C�GC�GC�C��C�C�C�C�GC�GC�C��C�C�C�C�C��C��C��C��C�C�GC��C��C�GC�GC�GC�GC�C�C�C��C�C��C�C�C��C�C�C�GC��C��C�C��C�C�C�GC��C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C��C�C�GC�C�C�C��C�GC�GC��zC��zC�C��C�GC�C��C��C��C�C�C�C�C�GC�GC�C�C�C�GC�C�C��C�C��C��C�C�D  �D �
D
D�pD
D�
D�D�pD�D�pD �D��D �D��D �D�pDpD�
D	 �D	�
D
pD
��DpD�
D �D�
D
D�
D
D�
D
D�
D
D�pDpD�pD
D��D
D�
D�D�pD
D�
D �D��D
D�
D �D��D �D��D�=D��D
D�
DpD�
D
D�pD
D�
D
D�pD  �D ��D!
D!�
D"
D"��D#
D#�
D$
D$�pD%
D%�pD&
D&�
D'
D'�
D(
D(�
D)
D)�pD*
D*��D+
D+�
D, �D,�
D-pD-�
D.
D.�pD/
D/��D0 �D0�
D1pD1�pD2
D2��D3 �D3�pD4
D4��D5
D5�pD6 �D6��D7
D7�pD8
D8��D9
D9�pD:
D:�pD;pD;�
D<
D<�pD= �D=�
D>pD>�pD?pD?��D@pD@�
DApDA�pDB
DB�
DC �DC�
DD
DD�pDE�DE�pDFpDF��DG �DG��DH �DH��DI
DI�pDJ �DJ�
DKpDK�
DL
DL�
DM
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
DR��DS
DS�pDT
DT��DU
DU�
DV
DV�pDW
DW�
DX�DX�pDYpDY�
DZ �DZ�pD[pD[�pD\
D\��D]
D]�
D^pD^�
D_ �D_��D`
D`�
Da
Da�
Db �Db��DcpDc�pDd
Dd�
DepDe�
Df �Df�
Dg
Dgz=Dh �Dh��Di �Di��Dj �Dj�
DkpDk�
Dl �Dlz=Dm �Dm�pDn
Dn�
Do
Do��Dp
Dp�pDqpDq�
Dr �Dr��Ds �Ds�
DtpDt�pDupDu�
Dv
Dv�
Dw
DwmpDye�D�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��mA���A�  A�A�A�A�  A���A���A���A���A���A���A�1A�bA�oA��A��A�{A�{A��A��A��A��A��A��A��A� �A��A��A�(�A�$�A�"�A�"�A� �A�"�A�"�A�"�A� �A��A�A���A��TA�hsA̧�A�E�A˺^Aˇ+A�M�A�{A�A���A��/A���A�C�A��A���AȁA�~�Aƣ�A���A�$�A���A�VA�bNA��FA�K�A�hsA��A��9A��HA���A�jA�7LA���A��7A��A�A�I�A�hsA�ĜA��A��A�O�A�ȴA�?}A�O�A��`A�p�A��A�bA��FA�-A�dZA�33A��yA� �A��#A���A���A�A�A� �A�ƨA��
A�jA�1A��A���A�~�A�ƨA�VA��!A��A��A��A���A���A�/A��#A�^Ay�mAt��ArbNAq/Aop�Aj��Ag�^Ae�-A`{A[hsAZ �AX-AV�AUXANbAKO�AH�AFM�AEK�ADĜACC�AB-AA7LA>ȴA;�A9��A6��A4�A4bA1�A/A.�jA,��A,z�A+��A+33A*E�A(ĜA'�A&n�A&bA%�mA%��A%�A$��A$(�A#?}A"=qA!p�A!�A!S�A��A��A�;AXA�A�AM�A�`A%AbNA�A
=A�9AK�A�A�#AoA�yA
�jA	�mA	S�A��AXA�A�A33A��A�`A�/A��A�DA5?AhsA��A��A/A �@�t�@��7@��@�^5@�  @��@�;d@�@��@�l�@�ff@���@��T@�o@�9@�1'@���@��m@��H@�Ĝ@���@�Ĝ@ڸR@��@ف@���@ش9@�bN@��@ם�@�@ԃ@��;@Ӆ@�J@Ѻ^@�x�@Гu@���@���@·+@�~�@�{@��@�1@�hs@Ǖ�@�l�@�t�@�dZ@�"�@���@��@�^5@�J@�J@�J@�O�@���@��w@�S�@��@��@��+@�@���@�hs@�X@�/@��@��j@�  @��@��@��\@�^5@�V@��@��T@�p�@�&�@��@�b@��
@�+@��T@�p�@�j@�1'@���@�+@���@�~�@�=q@�{@��#@�%@�A�@��m@���@�l�@�;d@�K�@�+@���@�ȴ@�ff@��@�Q�@�S�@�33@�S�@���@�X@�`B@�hs@�hs@��/@�bN@�(�@���@�ƨ@�t�@�S�@��@�^5@��@���@��T@�@��-@��7@�%@���@��
@�ȴ@�ff@�J@�{@�-@��7@�x�@�X@��@��`@���@�Ĝ@�V@�?}@�G�@�X@�&�@��`@���@�1'@��;@�K�@��@��@�^5@�@��@���@���@��-@�/@�j@�I�@�A�@�9X@�bN@�r�@�bN@�I�@�9X@��@��@��F@�
=@�E�@�$�@��@��-@�7L@�Ĝ@��u@��@�z�@�I�@���@���@�dZ@��y@��@���@���@���@�E�@�@���@��@�O�@��@��/@��u@�I�@�  @���@��F@�|�@�;d@��@��!@�E�@���@�@���@�x�@�G�@�Ĝ@�(�@��@��@�b@��@��;@��w@�K�@�33@��@��+@�ff@�J@��@�O�@�/@��/@�Z@�1'@���@�dZ@�33@�"�@�+@�"�@��@��@�"�@�o@���@��H@��@���@��!@���@��\@�5?@��T@���@��@�`B@�`B@�O�@�G�@�7L@�&�@��@��@�%@���@���@�r�@���@��@�o@�o@��H@��!@���@��+@�v�@�v�@�ff@�{@���@���@�8@z��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��HA��mA���A�  A�A�A�A�  A���A���A���A���A���A���A�1A�bA�oA��A��A�{A�{A��A��A��A��A��A��A��A� �A��A��A�(�A�$�A�"�A�"�A� �A�"�A�"�A�"�A� �A��A�A���A��TA�hsA̧�A�E�A˺^Aˇ+A�M�A�{A�A���A��/A���A�C�A��A���AȁA�~�Aƣ�A���A�$�A���A�VA�bNA��FA�K�A�hsA��A��9A��HA���A�jA�7LA���A��7A��A�A�I�A�hsA�ĜA��A��A�O�A�ȴA�?}A�O�A��`A�p�A��A�bA��FA�-A�dZA�33A��yA� �A��#A���A���A�A�A� �A�ƨA��
A�jA�1A��A���A�~�A�ƨA�VA��!A��A��A��A���A���A�/A��#A�^Ay�mAt��ArbNAq/Aop�Aj��Ag�^Ae�-A`{A[hsAZ �AX-AV�AUXANbAKO�AH�AFM�AEK�ADĜACC�AB-AA7LA>ȴA;�A9��A6��A4�A4bA1�A/A.�jA,��A,z�A+��A+33A*E�A(ĜA'�A&n�A&bA%�mA%��A%�A$��A$(�A#?}A"=qA!p�A!�A!S�A��A��A�;AXA�A�AM�A�`A%AbNA�A
=A�9AK�A�A�#AoA�yA
�jA	�mA	S�A��AXA�A�A33A��A�`A�/A��A�DA5?AhsA��A��A/A �@�t�@��7@��@�^5@�  @��@�;d@�@��@�l�@�ff@���@��T@�o@�9@�1'@���@��m@��H@�Ĝ@���@�Ĝ@ڸR@��@ف@���@ش9@�bN@��@ם�@�@ԃ@��;@Ӆ@�J@Ѻ^@�x�@Гu@���@���@·+@�~�@�{@��@�1@�hs@Ǖ�@�l�@�t�@�dZ@�"�@���@��@�^5@�J@�J@�J@�O�@���@��w@�S�@��@��@��+@�@���@�hs@�X@�/@��@��j@�  @��@��@��\@�^5@�V@��@��T@�p�@�&�@��@�b@��
@�+@��T@�p�@�j@�1'@���@�+@���@�~�@�=q@�{@��#@�%@�A�@��m@���@�l�@�;d@�K�@�+@���@�ȴ@�ff@��@�Q�@�S�@�33@�S�@���@�X@�`B@�hs@�hs@��/@�bN@�(�@���@�ƨ@�t�@�S�@��@�^5@��@���@��T@�@��-@��7@�%@���@��
@�ȴ@�ff@�J@�{@�-@��7@�x�@�X@��@��`@���@�Ĝ@�V@�?}@�G�@�X@�&�@��`@���@�1'@��;@�K�@��@��@�^5@�@��@���@���@��-@�/@�j@�I�@�A�@�9X@�bN@�r�@�bN@�I�@�9X@��@��@��F@�
=@�E�@�$�@��@��-@�7L@�Ĝ@��u@��@�z�@�I�@���@���@�dZ@��y@��@���@���@���@�E�@�@���@��@�O�@��@��/@��u@�I�@�  @���@��F@�|�@�;d@��@��!@�E�@���@�@���@�x�@�G�@�Ĝ@�(�@��@��@�b@��@��;@��w@�K�@�33@��@��+@�ff@�J@��@�O�@�/@��/@�Z@�1'@���@�dZ@�33@�"�@�+@�"�@��@��@�"�@�o@���@��H@��@���@��!@���@��\@�5?@��T@���@��@�`B@�`B@�O�@�G�@�7L@�&�@��@��@�%@���@���@�r�@���@��@�o@�o@��H@��!@���@��+@�v�@�v�@�ff@�{@���@���@�8@z��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
e`B
ffB
dZB
e`B
hsB
ffB
e`B
ffB
e`B
iyB
k�B
l�B
m�B
n�B
t�B
{�B
��B
�9B
�^B
�dB
�dB
�}B
��B
ÖB
��B
��B
�B
��B
��B	7B�B/BJ�BcTBv�B�RBÖB��B��BoBbB�B%�B7LB@�B?}B>wB>wB=qB=qB<jB;dBA�BG�BR�BgmBu�Bo�BffBW
BI�B>wB8RB1'B+B"�BVBB  B��B�B�/BƨB�qB�dB�RB�'B��B�PBy�Bk�BVBN�B@�B�B
��B
�fB
��B
�qB
�7B
iyB
G�B
33B
!�B	��B	�BB	��B	ĜB	�RB	��B	�JB	� B	cTB	F�B	<jB	33B	+B	"�B	1B�B�yB�ZB�NB�HB�/B�B�
B�B�B��BƨBȴBȴBŢBĜBŢB��B��B��B��B��B�B�HB�fB�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B��B��B��B�B�B�B�B�sB�fB�NB�B�5B�NB�/B��B��BɺBɺB��B��B��BÖBBBB��B��B�wB�jB�LB�?B�3B�'B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�!B�3B�?B�?B�FB�RB�RB�XB�RB�RB�LB�wB��BBɺB��B��B��B��B�B�B�
B�B��B�B�BB�ZB�`B�fB�fB�mB�B�yB�yB�B�B�B��B��B��B��B	B	B	+B	
=B	JB	VB	\B	hB	hB	�B	�B	�B	�B	�B	 �B	 �B	"�B	#�B	#�B	'�B	)�B	-B	.B	/B	49B	5?B	8RB	:^B	>wB	B�B	D�B	H�B	K�B	M�B	P�B	S�B	W
B	ZB	bNB	ffB	iyB	l�B	m�B	l�B	n�B	m�B	jB	k�B	jB	n�B	w�B	u�B	u�B	v�B	w�B	w�B	w�B	w�B	x�B	~�B	�B	�B	�%B	�+B	�7B	�=B	�DB	�PB	�\B	�bB	�oB	�uB	�uB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�9B	�?B	�XB	�dB	�jB	�wB	�}B	�wB	�wB	�}B	�}B	��B	��B	��B	B	ĜB	ĜB	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
B
%B
�B
s22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
e`B
ffB
dZB
e`B
hsB
ffB
e`B
ffB
e`B
iyB
k�B
l�B
m�B
n�B
t�B
{�B
��B
�9B
�^B
�dB
�dB
�}B
��B
ÖB
��B
��B
�B
��B
��B	7B�B/BJ�BcTBv�B�RBÖB��B��BoBbB�B%�B7LB@�B?}B>wB>wB=qB=qB<jB;dBA�BG�BR�BgmBu�Bo�BffBW
BI�B>wB8RB1'B+B"�BVBB  B��B�B�/BƨB�qB�dB�RB�'B��B�PBy�Bk�BVBN�B@�B�B
��B
�fB
��B
�qB
�7B
iyB
G�B
33B
!�B	��B	�BB	��B	ĜB	�RB	��B	�JB	� B	cTB	F�B	<jB	33B	+B	"�B	1B�B�yB�ZB�NB�HB�/B�B�
B�B�B��BƨBȴBȴBŢBĜBŢB��B��B��B��B��B�B�HB�fB�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B��B��B��B�B�B�B�B�sB�fB�NB�B�5B�NB�/B��B��BɺBɺB��B��B��BÖBBBB��B��B�wB�jB�LB�?B�3B�'B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�!B�3B�?B�?B�FB�RB�RB�XB�RB�RB�LB�wB��BBɺB��B��B��B��B�B�B�
B�B��B�B�BB�ZB�`B�fB�fB�mB�B�yB�yB�B�B�B��B��B��B��B	B	B	+B	
=B	JB	VB	\B	hB	hB	�B	�B	�B	�B	�B	 �B	 �B	"�B	#�B	#�B	'�B	)�B	-B	.B	/B	49B	5?B	8RB	:^B	>wB	B�B	D�B	H�B	K�B	M�B	P�B	S�B	W
B	ZB	bNB	ffB	iyB	l�B	m�B	l�B	n�B	m�B	jB	k�B	jB	n�B	w�B	u�B	u�B	v�B	w�B	w�B	w�B	w�B	x�B	~�B	�B	�B	�%B	�+B	�7B	�=B	�DB	�PB	�\B	�bB	�oB	�uB	�uB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�9B	�?B	�XB	�dB	�jB	�wB	�}B	�wB	�wB	�}B	�}B	��B	��B	��B	B	ĜB	ĜB	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
B
%B
�B
s22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191652                              AO  ARCAADJP                                                                    20181005191652    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191652  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191652  QCF$                G�O�G�O�G�O�8000            