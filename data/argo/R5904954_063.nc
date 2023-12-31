CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:03Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191703  20181005191703  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               ?A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׿%��1   @׿%�Jf@5_|�hs�dbM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      ?A   A   A   @9��@�  @�  A��A!��AA��A`  A�  A�  A�  A�33A�  A�  A�  A���B   B��B33B  B   B(  B0  B8  B@ffBHffBP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC�fC  C  C
  C�C  C  C  C  C  C�fC�fC  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C3�fC5�fC8  C:  C<  C>  C@  CB�CD  CE�fCH  CJ  CK�fCM��CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Ca�fCd  Ce�fCh  Cj�Cl  Cn�Cp  Cr  Ct�Cv  Cw��Cz  C|�C~�C�  C�  C�  C��C�  C��3C��3C��C��C��C�  C��3C�  C�  C��3C��3C��C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��C�  C��C��C��C��C�  C�  C�  C��C��C��3C��3C��C��C��C��C��C��3C��3C��3C�  C�  C��C��3C��3C��3C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C��C��C��C��C�  C��3C��C��C��C��C�  C��3C�  C�  C��3C��3C��3C��3C��3C�  C��3C�  C�  C�  C��C��C�  C��3D y�D  D� DfD�fD  D� DfDy�D  D� D��D�fD  D� DfD� D	  D	� D
  D
��D  Dy�D  D� DfD� D��Dy�D��D� D��D� DfD� D��D� D��Dy�D  Dy�D��D� DfD� D  Dy�D��D� D��Dy�D  D� D��Dy�D  D�fD  Dy�D��Dy�D��D� D fD � D ��D!� D"  D"� D#fD#�fD$  D$� D$��D%y�D&fD&y�D&��D'� D(fD(� D(��D)y�D)��D*y�D+  D+� D,  D,� D-  D-�fD.  D.y�D/  D/� D0  D0y�D1  D1�fD2  D2� D3fD3� D4  D4�fD5fD5� D6  D6� D7fD7� D7��D8y�D9  D9�fD:  D:� D;  D;� D<fD<�fD=  D=� D>  D>� D?fD?� D?�3D@y�DA  DA�fDB  DB� DB��DCy�DD  DD��DE  DEy�DF  DF� DGfDG� DG��DHs3DH��DIy�DJ  DJ�fDK  DK� DLfDL�fDM  DM�fDNfDN� DO  DO� DPfDP�fDQ  DQ� DRfDR� DS  DS� DTfDT� DU  DU�fDV  DV� DWfDW�fDX  DX� DYfDY�fDZ  DZy�D[  D[� D[��D\� D]  D]�fD^  D^�fD_  D_� D`fD`�fDa  Day�DbfDb�fDc  Dc�fDd�Dd� Dd��De� Df  Df� DgfDg�fDhfDh� Dh��Diy�DjfDj� Dk  Dk�fDlfDl� Dl��Dm� Dn  Dny�Dn��Doy�DpfDp� Dq  Dq�fDrfDr� Dr��Dsy�Ds��Dty�Dt��Du� DvfDv� Dw  Dw� Dw�3Dy�3D�H�D�\�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @C�@��@��A(�A$(�AD(�Ab�\A�G�A�G�A�G�A�z�A�G�A�G�A�G�A�{B ��B	p�B�
B��B ��B(��B0��B8��BA
=BI
=BP��BX��B`=qBh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B̅BЅB�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C\C\C(�C(�C
(�CB�C(�C(�C(�C(�C(�C\C\C(�C(�C (�C"(�C$B�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4\C6\C8(�C:(�C<(�C>(�C@(�CBB�CD(�CF\CH(�CJ(�CL\CM��CP(�CR(�CT(�CV(�CX(�CZ(�C\\C^(�C`(�Cb\Cd(�Cf\Ch(�CjB�Cl(�CnB�Cp(�Cr(�CtB�Cv(�Cw��Cz(�C|B�C~B�C�{C�{C�{C�!HC�{C��C��C�!HC�!HC�!HC�{C��C�{C�{C��C��C�!HC�{C�{C�!HC�{C��C�{C�{C�{C�!HC�{C�{C�{C�{C�!HC��C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�!HC�{C��C�{C�{C�!HC�{C�!HC�!HC�!HC�!HC�{C�{C�{C�!HC�!HC��C��C�.C�.C�!HC�!HC�!HC��C��C��C�{C�{C�!HC��C��C��C�{C�{C��C�{C�!HC�!HC�{C�{C�{C�{C�{C�{C�{C��C�{C�{C��C��C�{C�!HC�!HC�!HC�!HC�!HC�{C��C�!HC�!HC�!HC�!HC�{C��C�{C�{C��C��C��C��C��C�{C��C�{C�{C�{C�!HC�!HC�{D �D ��D
=D�=D�D��D
=D�=D�D��D
=D�=D�D��D
=D�=D�D�=D	
=D	�=D

=D
�
D
=D��D
=D�=D�D�=D�D��D�D�=D�D�=D�D�=D�D�=D�D��D
=D��D�D�=D�D�=D
=D��D�D�=D�D��D
=D�=D�D��D
=D��D
=D��D�D��D�D�=D �D �=D!�D!�=D"
=D"�=D#�D#��D$
=D$�=D%�D%��D&�D&��D'�D'�=D(�D(�=D)�D)��D*�D*��D+
=D+�=D,
=D,�=D-
=D-��D.
=D.��D/
=D/�=D0
=D0��D1
=D1��D2
=D2�=D3�D3�=D4
=D4��D5�D5�=D6
=D6�=D7�D7�=D8�D8��D9
=D9��D:
=D:�=D;
=D;�=D<�D<��D=
=D=�=D>
=D>�=D?�D?�=D?�pD@��DA
=DA��DB
=DB�=DC�DC��DD
=DD�
DE
=DE��DF
=DF�=DG�DG�=DH�DH}pDI�DI��DJ
=DJ��DK
=DK�=DL�DL��DM
=DM��DN�DN�=DO
=DO�=DP�DP��DQ
=DQ�=DR�DR�=DS
=DS�=DT�DT�=DU
=DU��DV
=DV�=DW�DW��DX
=DX�=DY�DY��DZ
=DZ��D[
=D[�=D\�D\�=D]
=D]��D^
=D^��D_
=D_�=D`�D`��Da
=Da��Db�Db��Dc
=Dc��Dd
Dd�=De�De�=Df
=Df�=Dg�Dg��Dh�Dh�=Di�Di��Dj�Dj�=Dk
=Dk��Dl�Dl�=Dm�Dm�=Dn
=Dn��Do�Do��Dp�Dp�=Dq
=Dq��Dr�Dr�=Ds�Ds��Dt�Dt��Du�Du�=Dv�Dv�=Dw
=Dw�=Dw�pDy�pD�ND�a�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�E�A�K�A�I�A�K�A�Q�A�S�A�S�A�Q�A�VA�\)A�XA�VA�;dA�E�A�M�A�;dA��A��A��A˼jA�&�A��A�XAƛ�AŮA�K�A��`A��A�%A£�A���A���A��`A���A��A�VA�1A��A�ĜA�ĜA�VA��A��yA��HA��-A�O�A���A�VA��A��mA�p�A���A�~�A�%A���A��FA���A�7LA�A�(�A��;A���A�{A���A�/A��!A�n�A�33A�S�A�M�A��A�=qA���A���A��uA��\A���A��`A�5?A��TA��A��A�A���A�-A�oA���A��FA��yA�7LA���A��A�^5A�z�A�A�v�A�bNA�{A~�!A}��A}�A|�!A{�AzAy
=Aw��At{Ar�ArVAq�Ap�DAoS�An{Al��AkoAg`BAe��Ad�!AaƨA_�mA_��A]�;AZr�AW�AV�+AT��AQG�AO�wANQ�AMp�AL��AK�AI&�ADȴAC��AC;dAB��A@�RA?dZA<��A:ȴA8ȴA6ffA4ĜA3��A3VA0~�A/�
A.�9A. �A-�A-�A*Q�A(n�A(bNA(�A'��A't�A&�+A$^5A#��A#�A#33A!��A!dZA��AO�A33A�!AJA�mA�A�+A�7A/A�A;dA��AA��A1'A\)A
�`A
�A
�A
�A
A�A	��A	`BA	%A�/A�A��AO�Al�A�A=qA�mA�7AȴA�A�A�!A��A �9A z�A Z@���@��9@���@�z�@��;@���@�+@��!@��+@�-@��h@�bN@�@�x�@�S�@�R@@�~�@�x�@�Z@�|�@�33@���@���@� �@�@�M�@�=q@�u@��@�7@��H@��@�7L@�z�@��
@�~�@�=q@�hs@�9X@�|�@�J@�V@ԓu@Ӿw@�n�@�V@��@щ7@�X@��`@Ѓ@�b@υ@���@Ώ\@�n�@�J@���@�hs@�I�@��@��y@��@�Z@Ǯ@ǝ�@ǅ@��
@���@ǶF@��y@Ƈ+@�M�@ũ�@�X@ģ�@�  @��@��@��
@+@�J@��^@��7@��@���@�t�@�K�@���@�$�@��-@��-@��`@�\)@���@���@��R@��y@�~�@��@���@�hs@���@�bN@�b@�S�@���@�~�@�ff@��@���@��h@�`B@��@���@�Ĝ@�z�@�1@�"�@���@�{@���@���@�?}@���@�I�@��
@�ƨ@���@�dZ@�S�@�;d@�33@��@���@���@�~�@�M�@�=q@�{@���@��@��@���@�7L@��/@��/@���@�Ĝ@��@�1'@���@�
=@���@���@�X@�Ĝ@���@�%@�X@��@�j@�ƨ@��H@�-@��^@��@�O�@�/@�/@���@��9@�z�@�r�@�j@�Z@�I�@�(�@���@��@�"�@�5?@���@�hs@��@�9X@�  @��@�t�@��H@���@�M�@�/@��@��@���@�Q�@�(�@�b@��m@�t�@�dZ@�\)@�\)@�dZ@�\)@��@�v�@�$�@��-@�hs@�x�@�X@�&�@��@���@��`@���@�I�@� �@�b@�1@���@���@���@��^@���@��@�`B@�`B@�hs@�p�@��@��7@�hs@�?}@�V@�Ĝ@��D@��@��@��@��D@��@��@�t�@�
=@�V@��T@��@��@��@��@���@���@��@� �@��P@�C�@�
=@�
=@��+@�=q@�J@�O�@��@��@�z�@��9@��@��`@��j@�j@�1'@��w@�\)@�"�@��y@�5?@�{@�{@��-@�p�@�?}@���@��/@��@���@~�@lĜ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�;dA�E�A�K�A�I�A�K�A�Q�A�S�A�S�A�Q�A�VA�\)A�XA�VA�;dA�E�A�M�A�;dA��A��A��A˼jA�&�A��A�XAƛ�AŮA�K�A��`A��A�%A£�A���A���A��`A���A��A�VA�1A��A�ĜA�ĜA�VA��A��yA��HA��-A�O�A���A�VA��A��mA�p�A���A�~�A�%A���A��FA���A�7LA�A�(�A��;A���A�{A���A�/A��!A�n�A�33A�S�A�M�A��A�=qA���A���A��uA��\A���A��`A�5?A��TA��A��A�A���A�-A�oA���A��FA��yA�7LA���A��A�^5A�z�A�A�v�A�bNA�{A~�!A}��A}�A|�!A{�AzAy
=Aw��At{Ar�ArVAq�Ap�DAoS�An{Al��AkoAg`BAe��Ad�!AaƨA_�mA_��A]�;AZr�AW�AV�+AT��AQG�AO�wANQ�AMp�AL��AK�AI&�ADȴAC��AC;dAB��A@�RA?dZA<��A:ȴA8ȴA6ffA4ĜA3��A3VA0~�A/�
A.�9A. �A-�A-�A*Q�A(n�A(bNA(�A'��A't�A&�+A$^5A#��A#�A#33A!��A!dZA��AO�A33A�!AJA�mA�A�+A�7A/A�A;dA��AA��A1'A\)A
�`A
�A
�A
�A
A�A	��A	`BA	%A�/A�A��AO�Al�A�A=qA�mA�7AȴA�A�A�!A��A �9A z�A Z@���@��9@���@�z�@��;@���@�+@��!@��+@�-@��h@�bN@�@�x�@�S�@�R@@�~�@�x�@�Z@�|�@�33@���@���@� �@�@�M�@�=q@�u@��@�7@��H@��@�7L@�z�@��
@�~�@�=q@�hs@�9X@�|�@�J@�V@ԓu@Ӿw@�n�@�V@��@щ7@�X@��`@Ѓ@�b@υ@���@Ώ\@�n�@�J@���@�hs@�I�@��@��y@��@�Z@Ǯ@ǝ�@ǅ@��
@���@ǶF@��y@Ƈ+@�M�@ũ�@�X@ģ�@�  @��@��@��
@+@�J@��^@��7@��@���@�t�@�K�@���@�$�@��-@��-@��`@�\)@���@���@��R@��y@�~�@��@���@�hs@���@�bN@�b@�S�@���@�~�@�ff@��@���@��h@�`B@��@���@�Ĝ@�z�@�1@�"�@���@�{@���@���@�?}@���@�I�@��
@�ƨ@���@�dZ@�S�@�;d@�33@��@���@���@�~�@�M�@�=q@�{@���@��@��@���@�7L@��/@��/@���@�Ĝ@��@�1'@���@�
=@���@���@�X@�Ĝ@���@�%@�X@��@�j@�ƨ@��H@�-@��^@��@�O�@�/@�/@���@��9@�z�@�r�@�j@�Z@�I�@�(�@���@��@�"�@�5?@���@�hs@��@�9X@�  @��@�t�@��H@���@�M�@�/@��@��@���@�Q�@�(�@�b@��m@�t�@�dZ@�\)@�\)@�dZ@�\)@��@�v�@�$�@��-@�hs@�x�@�X@�&�@��@���@��`@���@�I�@� �@�b@�1@���@���@���@��^@���@��@�`B@�`B@�hs@�p�@��@��7@�hs@�?}@�V@�Ĝ@��D@��@��@��@��D@��@��@�t�@�
=@�V@��T@��@��@��@��@���@���@��@� �@��P@�C�@�
=@�
=@��+@�=q@�J@�O�@��@��@�z�@��9@��@��`@��j@�j@�1'@��w@�\)@�"�@��y@�5?@�{@�{@��-@�p�@�?}@���@��/@��@���@~�@lĜ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨB�^B��B�#B�5B��BPB�B$�B,B33BB�BR�BW
BaHBn�Bs�B}�B�B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�1B}�Bz�Bu�Bl�Be`B]/B@�B)�B&�B�B�B�B�BPBB��B�BB��B�!B�oB�+B� BiyBL�B2-B+B\B
�fB
��B
ĜB
B
�^B
��B
�=B
|�B
t�B
n�B
iyB
e`B
bNB
]/B
VB
M�B
A�B
'�B
�B
�B
uB
PB
B	��B	�B	�5B	ÖB	�?B	��B	�{B	�B	�B	t�B	^5B	J�B	C�B	8RB	'�B	�B	�B	{B	bB		7B��B�yB�fB�TB�BB�B��B��BǮB��B�dB�LB�9B�'B�B�B�B��B��B��B��B��B�{B�oB�PB�=B�%B�B�+B�%B�B�B�B�B}�B|�B{�Bz�Bu�Bo�Bk�BhsBffBcTB`BB[#BQ�BK�BJ�BI�BI�BJ�BJ�BJ�BK�BJ�BI�BI�BI�BH�BM�BO�BM�BR�BR�BR�BVBZB_;BaHBbNBgmBp�Bw�By�B~�B�B�B�B�B�B�B� B~�B}�B}�B|�B{�B� B�=B�oB��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�?B�FB�RB�^B�^B�^B�^B�jB�qB�qB�}B��BBĜB��B��B��B��B��B��B��B��B�
B�B�B�B�#B�#B�#B�)B�)B�B�B��B��B��B�
B�B�#B�BB�`B�fB�mB�yB�B�B�B�B�B�B��B��B��B��B��B�B��B	B	B	B	%B	+B		7B	
=B	oB	�B	�B	�B	"�B	$�B	&�B	'�B	)�B	-B	.B	1'B	5?B	6FB	6FB	8RB	:^B	;dB	=qB	>wB	@�B	B�B	E�B	I�B	L�B	Q�B	S�B	W
B	_;B	`BB	bNB	cTB	ffB	ffB	hsB	jB	k�B	m�B	p�B	s�B	t�B	t�B	v�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�9B	�9B	�?B	�?B	�9B	�3B	�3B	�-B	�-B	�3B	�9B	�9B	�?B	�FB	�LB	�dB	�dB	�dB	�qB	�wB	�wB	�}B	��B	��B	B	B	ÖB	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�5B	�5B	�BB	�ZB	�`B	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
9B
�B
&2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨB�^B��B�#B�5B��BPB�B$�B,B33BB�BR�BW
BaHBn�Bs�B}�B�B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�1B}�Bz�Bu�Bl�Be`B]/B@�B)�B&�B�B�B�B�BPBB��B�BB��B�!B�oB�+B� BiyBL�B2-B+B\B
�fB
��B
ĜB
B
�^B
��B
�=B
|�B
t�B
n�B
iyB
e`B
bNB
]/B
VB
M�B
A�B
'�B
�B
�B
uB
PB
B	��B	�B	�5B	ÖB	�?B	��B	�{B	�B	�B	t�B	^5B	J�B	C�B	8RB	'�B	�B	�B	{B	bB		7B��B�yB�fB�TB�BB�B��B��BǮB��B�dB�LB�9B�'B�B�B�B��B��B��B��B��B�{B�oB�PB�=B�%B�B�+B�%B�B�B�B�B}�B|�B{�Bz�Bu�Bo�Bk�BhsBffBcTB`BB[#BQ�BK�BJ�BI�BI�BJ�BJ�BJ�BK�BJ�BI�BI�BI�BH�BM�BO�BM�BR�BR�BR�BVBZB_;BaHBbNBgmBp�Bw�By�B~�B�B�B�B�B�B�B� B~�B}�B}�B|�B{�B� B�=B�oB��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�?B�FB�RB�^B�^B�^B�^B�jB�qB�qB�}B��BBĜB��B��B��B��B��B��B��B��B�
B�B�B�B�#B�#B�#B�)B�)B�B�B��B��B��B�
B�B�#B�BB�`B�fB�mB�yB�B�B�B�B�B�B��B��B��B��B��B�B��B	B	B	B	%B	+B		7B	
=B	oB	�B	�B	�B	"�B	$�B	&�B	'�B	)�B	-B	.B	1'B	5?B	6FB	6FB	8RB	:^B	;dB	=qB	>wB	@�B	B�B	E�B	I�B	L�B	Q�B	S�B	W
B	_;B	`BB	bNB	cTB	ffB	ffB	hsB	jB	k�B	m�B	p�B	s�B	t�B	t�B	v�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�9B	�9B	�?B	�?B	�9B	�3B	�3B	�-B	�-B	�3B	�9B	�9B	�?B	�FB	�LB	�dB	�dB	�dB	�qB	�wB	�wB	�}B	��B	��B	B	B	ÖB	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�5B	�5B	�BB	�ZB	�`B	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
9B
�B
&2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191703                              AO  ARCAADJP                                                                    20181005191703    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191703  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191703  QCF$                G�O�G�O�G�O�8000            