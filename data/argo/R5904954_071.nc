CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:05Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191705  20181005191705  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               GA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��%ʗ1   @��%�n�@5C�
=p��d~��"�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      GA   A   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB33B��B'��B/��B8  B@  BHffBP��BXffB_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�33B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C�C�C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<�C>�C@  CA�fCD  CF  CH  CJ  CL�CN�CP  CR  CT�CV  CW�fCY�fC\  C^  C`�Cb�Cd  Cf  Ch  Cj�Cl  Cn�Cp�Cq�fCt  Cv  Cx  Cz  C|�C~�C��C�  C�  C�  C��3C�  C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C��C�  C�  C�  C��3C��3C��fC��C�  C�  C�  C��C��C��3C��3C��3C��3C��3C��3C��C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C��C�  C��C�  C�  C�  C�  C��3C�  C�  C��C��C��C��3C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��C��C�  C�  C�  C�  C��3C�  C��C��C��C�  C��C��C�  C��3C�  C�  C��3C��3C�  C��C�  C�  C��C��C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C�  D   D �fDfDy�D  D�fDfDy�D  D�fD  Dy�D  Dy�DfD�fDfD�fD	fD	�fD
fD
�fD
��Dy�D  D��D�D� D��Dy�D  Dy�D  D�fDfD� D  D� D  D� D  D�fD  D� D  Dy�D��D�fDfDy�D��D� D  Dy�D  D� D  Ds3D  D� D  D��DfD� D fD �fD!  D!�fD"  D"� D#fD#�fD#��D$y�D$��D%y�D%�3D&� D'fD'� D(  D(� D)  D)�fD*fD*� D+  D+�fD,fD,�fD,��D-� D.  D.�fD/�D/�fD0fD0� D1  D1� D1��D2y�D2��D3y�D3��D4� D5  D5y�D6  D6�fD7fD7� D8  D8� D8��D9� D9��D:y�D;  D;� D<fD<� D=  D=�fD>  D>y�D?  D?�fD@  D@� D@�3DA� DB  DBy�DC  DC�fDD  DDy�DE  DE�fDE��DFy�DF��DGy�DG��DH�fDI  DI� DJ  DJy�DK  DK� DL  DL�fDM  DM�fDNfDN�fDO  DOy�DP  DP� DP��DQ� DR  DR� DR��DSy�DT  DT�fDUfDU��DVfDV��DW  DWy�DXfDXy�DX��DY� DY��DZy�DZ�3Dg�Dg� DhfDh� Di  Diy�Di��Dj� DkfDk� Dl  Dl�fDmfDm� Dn  Dn�fDofDo� Do��Dpy�Dq  Dq� DrfDry�Dr��Ds� Dt  Dt� DufDu� Du��Dvy�Dw  Dw� Dw��Dy��D�E�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B�\B\)B B(B0B9(�BA(�BI�\BQ��BY�\B`Bi(�Bq(�By(�B��{B��{B��{B��{B��{B�aHB��{B��{B��{B�ǮB��{B��{B�ǮB�ǮB��{B��{B��{B�ǮBȔ{B�ǮB�ǮBԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=Cc�C
J=CJ=Cc�Cc�CJ=CJ=CJ=C0�CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4c�C6J=C8J=C:J=C<c�C>c�C@J=CB0�CDJ=CFJ=CHJ=CJJ=CLc�CNc�CPJ=CRJ=CTc�CVJ=CX0�CZ0�C\J=C^J=C`c�Cbc�CdJ=CfJ=ChJ=Cjc�ClJ=Cnc�Cpc�Cr0�CtJ=CvJ=CxJ=CzJ=C|c�C~c�C�1�C�%C�%C�%C�RC�%C�>�C�1�C�1�C�%C�RC�RC�%C�%C�%C�%C�%C�RC�RC�RC�%C�1�C�1�C�%C�%C�%C�RC�RC��C�1�C�%C�%C�%C�1�C�1�C�RC�RC�RC�RC�RC�RC�1�C�1�C�%C�%C�%C�%C�1�C�%C�RC�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�RC�%C�1�C�%C�1�C�%C�%C�%C�%C�RC�%C�%C�1�C�1�C�1�C�RC�%C�%C�%C�%C�%C�RC�%C�1�C�1�C�%C�1�C�1�C�%C�%C�%C�%C�RC�%C�1�C�1�C�1�C�%C�1�C�1�C�%C�RC�%C�%C�RC�RC�%C�1�C�%C�%C�1�C�1�C�%C�RC�%C�1�C�%C�%C�1�C�%C�%C�%C�%C�RC�%C�1�C�1�C�%D �D ��D�D�)D�D��D�D�)D�D��D�D�)D�D�)D�D��D�D��D	�D	��D
�D
��D)D�)D�D�\D\D��D)D�)D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D)D��D�D�)D)D��D�D�)D�D��D�D��D�D��D�D�\D�D��D �D ��D!�D!��D"�D"��D#�D#��D$)D$�)D%)D%�)D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-)D-��D.�D.��D/\D/��D0�D0��D1�D1��D2)D2�)D3)D3�)D4)D4��D5�D5�)D6�D6��D7�D7��D8�D8��D9)D9��D:)D:�)D;�D;��D<�D<��D=�D=��D>�D>�)D?�D?��D@�D@��DA�DA��DB�DB�)DC�DC��DD�DD�)DE�DE��DF)DF�)DG)DG�)DH)DH��DI�DI��DJ�DJ�)DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�)DP�DP��DQ)DQ��DR�DR��DS)DS�)DT�DT��DU�DU�\DV�DV�\DW�DW�)DX�DX�)DY)DY��DZ)DZ�)D[�Dg\Dg��Dh�Dh��Di�Di�)Dj)Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp)Dp�)Dq�Dq��Dr�Dr�)Ds)Ds��Dt�Dt��Du�Du��Dv)Dv�)Dw�Dw��Dw�\Dy�{D�OD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�oA��A��A��A��A��A��A��A� �A� �A�"�A�$�A�&�A�&�A�(�A�(�Aں^A��Aֺ^A�t�A�5?A���A�-A��
A�1Ař�A�;dA�JA�&�A�|�A�M�A���A�E�A�1'A��A��A���A�r�A��`A�ZA�VA�A���A�bNA�A���A�G�A���A��^A�  A�I�A��wA�/A��FA��A���A���A��mA�ffA�p�A�+A��A�A���A��PA�A�A��jA�^5A�JA�z�A�E�A���A��-A�r�A��A�~�A�-A�C�A�ffA�bA���A��TA�A�oA��A� �A�G�A�9XA���A��9A�v�A��wA���A��A�+A|�jAz�Axv�Aw�Av�`ArbNAo�Ak��Ah��Ae��AdA�Ac��AcS�Ac+Ab��Aa��A_x�A\�RA\z�A\bA[�FA[�AZ  AYVAU��AQ\)AP�\API�AP �AO33AL-AI�^AG�hAD�A@�`A@1'A?
=A=\)A<��A<bNA:��A:1'A9�A7��A7
=A6�\A5`BA4��A4~�A3l�A2�A2�A1�;A0��A0{A/�^A/x�A.��A,�RA*��A)�A)��A)?}A(�A(��A(^5A'�TA&ffA#��A"~�A!`BA �`A �/A ZA�`A�A�-AjAl�A9XA�
AK�A�9A1'A�TA��A&�A5?A�9A��A��A	hsA�\A�A��A�`A�\AA�A�
A�A�A ��A ��A �!A �D@��;@�=q@���@�M�@�?}@�l�@�V@���@��#@��^@�@���@�&�@�r�@��y@�v�@�=q@�@�j@�R@�p�@�D@�+@蛦@�-@�Z@�33@�7L@��`@��@ޟ�@ݡ�@݁@�X@�V@��@�p�@�{@�C�@�A�@ߕ�@�$�@܃@�;d@���@�^5@�9X@�\)@ְ!@�ff@�^5@�n�@թ�@�z�@�  @с@�j@�  @��;@�o@�=q@�@�p�@�X@�%@�1'@��`@��@�@���@�@őh@�X@��`@�Q�@å�@���@�ff@��#@��@�(�@��
@�t�@�33@�o@���@�$�@���@��-@�&�@��;@�o@�J@�@���@��h@�?}@��@���@��@�r�@�A�@��
@�33@��H@���@�X@��/@�G�@�X@��@�1'@��!@���@�V@���@�z�@�bN@�z�@���@���@��@�/@�V@��@�1'@���@�|�@���@�J@���@�hs@�G�@�7L@�?}@�V@��/@��9@�A�@��w@��@�t�@�dZ@��H@�E�@�J@���@���@�7L@��@��@�S�@�l�@�ff@��T@��@�@��@��@���@�Q�@�(�@�9X@���@�
=@�-@�5?@�{@���@��^@��^@��-@��-@�x�@�/@��`@��j@��@�z�@�bN@�1@���@�;d@�"�@�
=@�@��y@��R@���@�~�@�E�@�J@�@���@�@�hs@�O�@�O�@�&�@��/@��/@���@���@�9X@�(�@��@�ƨ@�33@�ȴ@���@���@�^5@�@�p�@�?}@���@�bN@�(�@���@��F@�dZ@�K�@�+@��R@�-@�5?@�{@���@��@�O�@�%@��9@��D@�z�@�1'@���@��@�x�@�x�@�hs@���@���@��u@��@�z�@�bN@�1'@��m@��w@�K�@�o@���@�ȴ@�M�@��T@�O�@�V@��@���@���@�z�@�A�@�A�@�9X@�1@���@�S�@��H@���@��+@�(�@u�@eT�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�
=A�oA��A��A��A��A��A��A��A� �A� �A�"�A�$�A�&�A�&�A�(�A�(�Aں^A��Aֺ^A�t�A�5?A���A�-A��
A�1Ař�A�;dA�JA�&�A�|�A�M�A���A�E�A�1'A��A��A���A�r�A��`A�ZA�VA�A���A�bNA�A���A�G�A���A��^A�  A�I�A��wA�/A��FA��A���A���A��mA�ffA�p�A�+A��A�A���A��PA�A�A��jA�^5A�JA�z�A�E�A���A��-A�r�A��A�~�A�-A�C�A�ffA�bA���A��TA�A�oA��A� �A�G�A�9XA���A��9A�v�A��wA���A��A�+A|�jAz�Axv�Aw�Av�`ArbNAo�Ak��Ah��Ae��AdA�Ac��AcS�Ac+Ab��Aa��A_x�A\�RA\z�A\bA[�FA[�AZ  AYVAU��AQ\)AP�\API�AP �AO33AL-AI�^AG�hAD�A@�`A@1'A?
=A=\)A<��A<bNA:��A:1'A9�A7��A7
=A6�\A5`BA4��A4~�A3l�A2�A2�A1�;A0��A0{A/�^A/x�A.��A,�RA*��A)�A)��A)?}A(�A(��A(^5A'�TA&ffA#��A"~�A!`BA �`A �/A ZA�`A�A�-AjAl�A9XA�
AK�A�9A1'A�TA��A&�A5?A�9A��A��A	hsA�\A�A��A�`A�\AA�A�
A�A�A ��A ��A �!A �D@��;@�=q@���@�M�@�?}@�l�@�V@���@��#@��^@�@���@�&�@�r�@��y@�v�@�=q@�@�j@�R@�p�@�D@�+@蛦@�-@�Z@�33@�7L@��`@��@ޟ�@ݡ�@݁@�X@�V@��@�p�@�{@�C�@�A�@ߕ�@�$�@܃@�;d@���@�^5@�9X@�\)@ְ!@�ff@�^5@�n�@թ�@�z�@�  @с@�j@�  @��;@�o@�=q@�@�p�@�X@�%@�1'@��`@��@�@���@�@őh@�X@��`@�Q�@å�@���@�ff@��#@��@�(�@��
@�t�@�33@�o@���@�$�@���@��-@�&�@��;@�o@�J@�@���@��h@�?}@��@���@��@�r�@�A�@��
@�33@��H@���@�X@��/@�G�@�X@��@�1'@��!@���@�V@���@�z�@�bN@�z�@���@���@��@�/@�V@��@�1'@���@�|�@���@�J@���@�hs@�G�@�7L@�?}@�V@��/@��9@�A�@��w@��@�t�@�dZ@��H@�E�@�J@���@���@�7L@��@��@�S�@�l�@�ff@��T@��@�@��@��@���@�Q�@�(�@�9X@���@�
=@�-@�5?@�{@���@��^@��^@��-@��-@�x�@�/@��`@��j@��@�z�@�bN@�1@���@�;d@�"�@�
=@�@��y@��R@���@�~�@�E�@�J@�@���@�@�hs@�O�@�O�@�&�@��/@��/@���@���@�9X@�(�@��@�ƨ@�33@�ȴ@���@���@�^5@�@�p�@�?}@���@�bN@�(�@���@��F@�dZ@�K�@�+@��R@�-@�5?@�{@���@��@�O�@�%@��9@��D@�z�@�1'@���@��@�x�@�x�@�hs@���@���@��u@��@�z�@�bN@�1'@��m@��w@�K�@�o@���@�ȴ@�M�@��T@�O�@�V@��@���@���@�z�@�A�@�A�@�9X@�1@���@�S�@��H@���@��+@�(�@u�@eT�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B�qB�dB�jB�/B�B%BDBuB+BF�BT�B^5BgmBiyBo�B�B�oB�{B�uB��B�B�3B�LB�wB��BBĜBƨBȴBɺB��B��B��B��BÖB�wB�^B�'B��B�\B|�BjB_;B[#BS�BG�B1'B{B��B�HB�B��B�XB��B��B��B�PB�B}�Bx�BjB[#B:^B&�BbBB
��B
�;B
ǮB
�'B
��B
��B
��B
�uB
�+B
v�B
dZB
I�B
-B
�B
\B
	7B	��B	�fB	��B	�qB	��B	�bB	�+B	�B	�B	�B	� B	{�B	s�B	hsB	ffB	bNB	`BB	[#B	S�B	K�B	=qB	&�B	"�B	 �B	�B	�B	
=B��B�B�B�/B�B��B��B��B��BƨBĜB��B�dB�RB�?B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�=B�7B�bB�oB�hB�\B�VB�%B�B}�B|�B~�B� B�B�+B�oB�B�B~�Bp�Be`B`BBS�BE�BA�B@�B?}B?}BB�BB�BB�BD�BE�BE�BF�BF�BF�BI�BP�BW
BT�BR�BXB_;Be`Bt�Bx�By�Bx�Bw�Bv�Bx�Bx�Bx�Bx�By�B|�B�B�B� Bz�Bz�Bw�Bt�Bt�Bt�Bv�B{�B�B�B�B�B�DB��B��B�B�FB�XB�XB�wB��B��B��BÖBĜBƨBǮBǮBȴB��B��BɺB��B��B��B��B��B��B��B��B��B��B��B�#B�5B�/B�/B�/B�)B�)B�/B�;B�HB�ZB�fB�yB�B�B�B�B�B�B��B��B��B��B��B	B	+B	PB	\B	bB	hB	�B	�B	�B	�B	!�B	"�B	$�B	&�B	'�B	'�B	%�B	(�B	.B	0!B	2-B	1'B	/B	/B	0!B	0!B	1'B	5?B	9XB	<jB	?}B	@�B	@�B	B�B	E�B	G�B	J�B	L�B	K�B	N�B	R�B	R�B	R�B	S�B	T�B	VB	W
B	XB	\)B	]/B	^5B	aHB	dZB	gmB	jB	k�B	l�B	m�B	r�B	u�B	u�B	}�B	�B	� B	� B	�B	�B	�+B	�7B	�DB	�JB	�\B	�oB	�oB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�9B	�9B	�?B	�LB	�RB	�RB	�XB	�^B	�^B	�^B	�dB	�qB	�qB	�wB	�}B	��B	ÖB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�#B	�#B	�)B	�5B	�BBB�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B
	RB
jB
,�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B�qB�dB�jB�/B�B%BDBuB+BF�BT�B^5BgmBiyBo�B�B�oB�{B�uB��B�B�3B�LB�wB��BBĜBƨBȴBɺB��B��B��B��BÖB�wB�^B�'B��B�\B|�BjB_;B[#BS�BG�B1'B{B��B�HB�B��B�XB��B��B��B�PB�B}�Bx�BjB[#B:^B&�BbBB
��B
�;B
ǮB
�'B
��B
��B
��B
�uB
�+B
v�B
dZB
I�B
-B
�B
\B
	7B	��B	�fB	��B	�qB	��B	�bB	�+B	�B	�B	�B	� B	{�B	s�B	hsB	ffB	bNB	`BB	[#B	S�B	K�B	=qB	&�B	"�B	 �B	�B	�B	
=B��B�B�B�/B�B��B��B��B��BƨBĜB��B�dB�RB�?B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�=B�7B�bB�oB�hB�\B�VB�%B�B}�B|�B~�B� B�B�+B�oB�B�B~�Bp�Be`B`BBS�BE�BA�B@�B?}B?}BB�BB�BB�BD�BE�BE�BF�BF�BF�BI�BP�BW
BT�BR�BXB_;Be`Bt�Bx�By�Bx�Bw�Bv�Bx�Bx�Bx�Bx�By�B|�B�B�B� Bz�Bz�Bw�Bt�Bt�Bt�Bv�B{�B�B�B�B�B�DB��B��B�B�FB�XB�XB�wB��B��B��BÖBĜBƨBǮBǮBȴB��B��BɺB��B��B��B��B��B��B��B��B��B��B��B�#B�5B�/B�/B�/B�)B�)B�/B�;B�HB�ZB�fB�yB�B�B�B�B�B�B��B��B��B��B��B	B	+B	PB	\B	bB	hB	�B	�B	�B	�B	!�B	"�B	$�B	&�B	'�B	'�B	%�B	(�B	.B	0!B	2-B	1'B	/B	/B	0!B	0!B	1'B	5?B	9XB	<jB	?}B	@�B	@�B	B�B	E�B	G�B	J�B	L�B	K�B	N�B	R�B	R�B	R�B	S�B	T�B	VB	W
B	XB	\)B	]/B	^5B	aHB	dZB	gmB	jB	k�B	l�B	m�B	r�B	u�B	u�B	}�B	�B	� B	� B	�B	�B	�+B	�7B	�DB	�JB	�\B	�oB	�oB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�9B	�9B	�?B	�LB	�RB	�RB	�XB	�^B	�^B	�^B	�dB	�qB	�qB	�wB	�}B	��B	ÖB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�#B	�#B	�)B	�5B	�BBB�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B
	RB
jB
,�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191705                              AO  ARCAADJP                                                                    20181005191705    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191705  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191705  QCF$                G�O�G�O�G�O�8000            