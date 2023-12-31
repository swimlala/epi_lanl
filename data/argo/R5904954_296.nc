CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:56Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191756  20181005191756  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              (A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��dޠZ1   @��ey\��@5BM����d�x���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     (A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*�C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C��C��C�  C��3C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C��3C��3C��3C��3C�  C��C�  C��3C��3C�  C��C��C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C��C��C��C��3C�  C�  C��3D y�D  D�fD  Dy�D��D� D  Dy�D��D�fD  D�fD  D� DfD� D��D	�fD
  D
�fDfD� D��D� DfD� D  D� D  D�fD��D� D  D� D��D� DfD�fD  Dy�DfD�fDfDy�DfD��D  Dy�D��Ds3D�3D� D  Dy�D  D�fD  Dy�DfD� DfD� D   D � D ��D!� D"  D"� D"��D#�fD$fD$� D%  D%y�D&fD&� D'  D'� D(fD(y�D)  D)� D)��D*� D+fD+� D,  D,� D-  D-� D-��D.� D.��D/y�D/��D0� D1  D1y�D1��D2�fD3  D3�fD4fD4� D5  D5��D6fD6�fD7fD7� D8fD8�fD9fD9�fD:  D:y�D:��D;� D<  D<y�D=fD=�fD>�D>y�D?fD?� D@  D@�fDA  DAy�DA��DB�fDB��DC� DD  DDy�DE  DE� DF  DFy�DF��DG� DHfDH�fDH��DI� DJfDJ� DJ��DK�fDK��DL�fDM  DMy�DN  DN� DO  DO�fDO��DP� DQ�DQ�fDRfDR� DS  DSy�DS��DTy�DU  DU�fDV  DV� DWfDW��DX  DXy�DX��DYy�DZ  DZ� D[  D[� D\�D\� D\��D]� D^fD^�fD_  D_�fD`  D`� Da  Da� Db  Db�fDc  Dcy�DdfDd� Dd��Dey�De��Df�fDg  Dgy�Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dq��Dry�Ds  Ds�fDt  Dty�Dt��Du� Dv  Dv� Dv��Dwy�Dw��Dy� D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @E@�G�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B�ǮB��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�ǮB�ǮB�ǮB��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&0�C(J=C*c�C,J=C.J=C0J=C2J=C4J=C6c�C8c�C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=Clc�CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�RC�%C�RC�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�RC�%C�%C�%C�%C�RC�RC�RC�RC�RC�%C�%C�%C�%C�RC�%C�%C�RC�%C�%C�RC�RC�%C�RC�%C�%C�%C�%C�%C�%C�%C�RC�RC�RC�%C�1�C�1�C�1�C�%C�RC�RC�RC�RC�RC�%C�1�C�%C�%C�%C�%C�1�C�1�C�1�C�%C�RC�%C�%C�RC�RC�RC�RC�%C�1�C�%C�RC�RC�%C�1�C�1�C�%C�RC�RC�%C�1�C�%C�%C�RC�RC�%C�1�C�1�C�1�C�RC�%C�%D )D �)D�D��D�D�)D)D��D�D�)D)D��D�D��D�D��D�D��D	)D	��D
�D
��D�D��D)D��D�D��D�D��D�D��D)D��D�D��D)D��D�D��D�D�)D�D��D�D�)D�D�\D�D�)D)D��D�D��D�D�)D�D��D�D�)D�D��D�D��D �D ��D!)D!��D"�D"��D#)D#��D$�D$��D%�D%�)D&�D&��D'�D'��D(�D(�)D)�D)��D*)D*��D+�D+��D,�D,��D-�D-��D.)D.��D/)D/�)D0)D0��D1�D1�)D2)D2��D3�D3��D4�D4��D5�D5�\D6�D6��D7�D7��D8�D8��D9�D9��D:�D:�)D;)D;��D<�D<�)D=�D=��D>\D>�)D?�D?��D@�D@��DA�DA�)DB)DB��DC)DC��DD�DD�)DE�DE��DF�DF�)DG)DG��DH�DH��DI)DI��DJ�DJ��DK)DK��DL)DL��DM�DM�)DN�DN��DO�DO��DP)DP��DQ\DQ��DR�DR��DS�DS�)DT)DT�)DU�DU��DV�DV��DW�DW�\DX�DX�)DY)DY�)DZ�DZ��D[�D[��D\\D\��D])D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�)Dd�Dd��De)De�)Df)Df��Dg�Dg�)Dh�Dh��Di)Di��Dj�Dj��Dk�Dk��Dl)Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr)Dr�)Ds�Ds��Dt�Dt�)Du)Du��Dv�Dv��Dw)Dw�)Dw�\Dy��D�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AĮAİ!AĬAĮAĬAİ!AĲ-Aĝ�AĬAħ�A�{A+A���A��7A�;dA�?}A�A���A�jA��A��PA�M�A��A���A�ffA�?}A�oA��yA��-A���A��DA��A�z�A�r�A�n�A�O�A�/A�bA��A�"�A�+A�JA���A��jA��FA��9A�`BA�1A�A���A�Q�A��/A�7LA�ĜA�A�A��A��uA�XA��PA���A��PA��mA�&�A�z�A��PA��A���A�"�A���A�E�A��
A���A�|�A�G�A��wA���A���A�hsA��;A��A�;dA���A�K�A�VA��A���A�|�A�z�A�;dA��RA�ƨA�A�A�-A��;A�ZA��DA��!A��FA���A�VA��yA�-A�-A�33A�ĜA�ZA��9A�{A�A�A�v�A��A�E�A��9A�l�A��hA�ffA���A�bA?}A~�A}�A|�+Ay`BAw��Av{At��AtM�At9XAs�FAs+ArbNAq��Ap$�An��Am7LAlbAk��Ai��Af�Ac�
Ab �A_�A\�yAZ��AW��AV��AT��AT  AS�FAR5?APE�AO�-AN��AN1'AMG�AL�uAKp�AJ=qAH��AF��AE?}ADM�AC�TACG�AB�jAA�AA?}A?��A>JA<�A:��A:M�A:(�A9�mA9�A6n�A4�A3�PA1�;A0^5A/��A/�A/&�A-��A+oA)�#A(ȴA(A�A(�A'�TA'%A%XA$�A"A�A!�A ZA;dAQ�A��A��A��AƨA �A��Az�A�A�#A��AhsAVAVA(�A{A��A�\At�A��A�/AhsA
��A
�uA	p�A�!A
=A5?A��A��@��
@�&�@���@��+@�=q@�@�hs@���@��@�bN@���@�hs@���@�(�@�"�@�@�&�@�9@�/@�?}@���@��m@@�dZ@�33@��@�h@�r�@�V@�ff@��#@��@�dZ@�@�9X@�ȴ@��m@�Ĝ@ӝ�@�33@�n�@���@·+@�p�@��/@̋D@��@�;d@���@ʇ+@�ff@�@ɉ7@�7L@�r�@�o@���@�ff@�X@��/@Å@���@���@���@�hs@���@��@�E�@�$�@���@���@��-@���@�X@��D@��F@�\)@��@��H@���@��@���@��h@���@�x�@�%@��@�M�@���@��7@�`B@�?}@��@�%@���@�bN@�9X@�b@��F@�o@��\@�$�@��@�1@���@�ȴ@��y@�o@�?}@�p�@��@�bN@��m@�\)@�ȴ@���@�J@���@�O�@�?}@��@��/@��j@�z�@�1@���@�C�@���@���@�~�@�=q@��#@�`B@�&�@�%@���@��/@�bN@�ƨ@���@�;d@���@��+@��7@��@�r�@�1'@��@���@�S�@�
=@��!@�v�@�^5@�M�@�V@�M�@��@�{@�{@�$�@�@���@��@�&�@�7L@�/@�&�@��@��@���@���@��u@� �@��
@�|�@�dZ@�S�@�+@���@�$�@��T@��^@�hs@�/@�V@���@���@��@��`@��/@���@��j@� �@��@�\)@�K�@�33@���@��y@�ȴ@�~�@��@�`B@�/@��/@�1'@���@��P@�\)@�K�@�@���@���@�+@�"�@�
=@��@�n�@��^@���@�1'@���@�@���@���@��R@��!@���@���@�v�@�ff@�V@�5?@�J@��#@���@��7@�X@�G�@���@���@���@���@��9@��D@��m@��@�+@�
=@�@��H@���@�ff@�$�@�{@�{@�J@�@��^@���@�p�@�%@��`@���@��D@��D@���@���@���@{�}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AĮAİ!AĬAĮAĬAİ!AĲ-Aĝ�AĬAħ�A�{A+A���A��7A�;dA�?}A�A���A�jA��A��PA�M�A��A���A�ffA�?}A�oA��yA��-A���A��DA��A�z�A�r�A�n�A�O�A�/A�bA��A�"�A�+A�JA���A��jA��FA��9A�`BA�1A�A���A�Q�A��/A�7LA�ĜA�A�A��A��uA�XA��PA���A��PA��mA�&�A�z�A��PA��A���A�"�A���A�E�A��
A���A�|�A�G�A��wA���A���A�hsA��;A��A�;dA���A�K�A�VA��A���A�|�A�z�A�;dA��RA�ƨA�A�A�-A��;A�ZA��DA��!A��FA���A�VA��yA�-A�-A�33A�ĜA�ZA��9A�{A�A�A�v�A��A�E�A��9A�l�A��hA�ffA���A�bA?}A~�A}�A|�+Ay`BAw��Av{At��AtM�At9XAs�FAs+ArbNAq��Ap$�An��Am7LAlbAk��Ai��Af�Ac�
Ab �A_�A\�yAZ��AW��AV��AT��AT  AS�FAR5?APE�AO�-AN��AN1'AMG�AL�uAKp�AJ=qAH��AF��AE?}ADM�AC�TACG�AB�jAA�AA?}A?��A>JA<�A:��A:M�A:(�A9�mA9�A6n�A4�A3�PA1�;A0^5A/��A/�A/&�A-��A+oA)�#A(ȴA(A�A(�A'�TA'%A%XA$�A"A�A!�A ZA;dAQ�A��A��A��AƨA �A��Az�A�A�#A��AhsAVAVA(�A{A��A�\At�A��A�/AhsA
��A
�uA	p�A�!A
=A5?A��A��@��
@�&�@���@��+@�=q@�@�hs@���@��@�bN@���@�hs@���@�(�@�"�@�@�&�@�9@�/@�?}@���@��m@@�dZ@�33@��@�h@�r�@�V@�ff@��#@��@�dZ@�@�9X@�ȴ@��m@�Ĝ@ӝ�@�33@�n�@���@·+@�p�@��/@̋D@��@�;d@���@ʇ+@�ff@�@ɉ7@�7L@�r�@�o@���@�ff@�X@��/@Å@���@���@���@�hs@���@��@�E�@�$�@���@���@��-@���@�X@��D@��F@�\)@��@��H@���@��@���@��h@���@�x�@�%@��@�M�@���@��7@�`B@�?}@��@�%@���@�bN@�9X@�b@��F@�o@��\@�$�@��@�1@���@�ȴ@��y@�o@�?}@�p�@��@�bN@��m@�\)@�ȴ@���@�J@���@�O�@�?}@��@��/@��j@�z�@�1@���@�C�@���@���@�~�@�=q@��#@�`B@�&�@�%@���@��/@�bN@�ƨ@���@�;d@���@��+@��7@��@�r�@�1'@��@���@�S�@�
=@��!@�v�@�^5@�M�@�V@�M�@��@�{@�{@�$�@�@���@��@�&�@�7L@�/@�&�@��@��@���@���@��u@� �@��
@�|�@�dZ@�S�@�+@���@�$�@��T@��^@�hs@�/@�V@���@���@��@��`@��/@���@��j@� �@��@�\)@�K�@�33@���@��y@�ȴ@�~�@��@�`B@�/@��/@�1'@���@��P@�\)@�K�@�@���@���@�+@�"�@�
=@��@�n�@��^@���@�1'@���@�@���@���@��R@��!@���@���@�v�@�ff@�V@�5?@�J@��#@���@��7@�X@�G�@���@���@���@���@��9@��D@��m@��@�+@�
=@�@��H@���@�ff@�$�@�{@�{@�J@�@��^@���@�p�@�%@��`@���@��D@��D@���@���@���@{�}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�BbBDB�B33B:^B>wBC�BI�BO�BP�BS�BW
BYBZB]/B_;BbNBcTBaHBaHBaHBaHBcTBcTBcTBe`BiyBv�B|�B�B�VB�bB�bB�bB��B��B��B��B��B��B��B�hB�JB�1B�B�Bx�Bv�B�bB�B�XB�qB�qB�dB��B��B��B��B��B�PB�B�B� Bs�B\)BS�BN�BaHB^5BYBS�BL�B=qB8RB33B+B-B!�B�BoB��B�B�B��B�JB� Bp�BgmBbNBZBL�B>wB.B&�B�BuB	7B
��B
��B
�B
�ZB
�;B
��B
B
�-B
��B
��B
��B
�bB
�1B
t�B
iyB
`BB
W
B
R�B
Q�B
M�B
G�B
A�B
?}B
5?B
+B
�B
{B
PB	��B	�HB	��B	ĜB	�B	��B	~�B	k�B	aHB	W
B	P�B	L�B	C�B	=qB	B�B	G�B	E�B	A�B	=qB	7LB	0!B	(�B	�B	�B	uB	hB	VB	
=B	%B	  B��B�mB�5B�B��B��B��BǮB�}B�XB�9B�B��B��B��B��B��B��B�oB�\B�VB�PB�DB�1B�B� B|�By�Bv�Bt�Bp�Bm�BffBbNB_;B\)BZBZBYBYBXBW
BXB[#B\)B[#B[#B_;B^5B^5B[#BXBVBS�BN�BJ�BI�BI�BH�BF�BB�B>wB<jB<jB;dB;dB;dB;dB;dB=qBL�BO�BXBZBYBW
BR�BT�B\)BaHBbNBgmBhsBhsBiyBjBo�Bq�Bq�Bm�BcTBaHBbNBcTBcTB`BBbNBffBhsBjBk�BjBu�Bz�B}�B~�B�B�B�B�%B�%B�+B�7B�=B�hB��B��B�B�'B�'B�?B�FB�RB�XB�^B�XB�XB�jB�qB�qB�qB�qBĜBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�)B�;B�HB�TB�mB�B�B��B��B	B		7B	{B	�B	�B	!�B	%�B	,B	/B	33B	49B	6FB	6FB	7LB	:^B	=qB	?}B	A�B	C�B	G�B	J�B	M�B	O�B	P�B	R�B	VB	ZB	[#B	]/B	`BB	bNB	bNB	cTB	cTB	cTB	bNB	dZB	jB	n�B	q�B	s�B	t�B	t�B	u�B	x�B	{�B	|�B	}�B	�B	�=B	�PB	�PB	�VB	�\B	�hB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�?B	�XB	�dB	�qB	�}B	��B	��B	ÖB	ƨB	ƨB	ɺB	ɺB	ȴB	ǮB	ɺB	��B	��B	��B	��B	�
B	�)B	�;B	�;B	�BB	�BB	�NB	�TB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B�B�B�B�B�B�B�B�B�B�BbBDB�B33B:^B>wBC�BI�BO�BP�BS�BW
BYBZB]/B_;BbNBcTBaHBaHBaHBaHBcTBcTBcTBe`BiyBv�B|�B�B�VB�bB�bB�bB��B��B��B��B��B��B��B�hB�JB�1B�B�Bx�Bv�B�bB�B�XB�qB�qB�dB��B��B��B��B��B�PB�B�B� Bs�B\)BS�BN�BaHB^5BYBS�BL�B=qB8RB33B+B-B!�B�BoB��B�B�B��B�JB� Bp�BgmBbNBZBL�B>wB.B&�B�BuB	7B
��B
��B
�B
�ZB
�;B
��B
B
�-B
��B
��B
��B
�bB
�1B
t�B
iyB
`BB
W
B
R�B
Q�B
M�B
G�B
A�B
?}B
5?B
+B
�B
{B
PB	��B	�HB	��B	ĜB	�B	��B	~�B	k�B	aHB	W
B	P�B	L�B	C�B	=qB	B�B	G�B	E�B	A�B	=qB	7LB	0!B	(�B	�B	�B	uB	hB	VB	
=B	%B	  B��B�mB�5B�B��B��B��BǮB�}B�XB�9B�B��B��B��B��B��B��B�oB�\B�VB�PB�DB�1B�B� B|�By�Bv�Bt�Bp�Bm�BffBbNB_;B\)BZBZBYBYBXBW
BXB[#B\)B[#B[#B_;B^5B^5B[#BXBVBS�BN�BJ�BI�BI�BH�BF�BB�B>wB<jB<jB;dB;dB;dB;dB;dB=qBL�BO�BXBZBYBW
BR�BT�B\)BaHBbNBgmBhsBhsBiyBjBo�Bq�Bq�Bm�BcTBaHBbNBcTBcTB`BBbNBffBhsBjBk�BjBu�Bz�B}�B~�B�B�B�B�%B�%B�+B�7B�=B�hB��B��B�B�'B�'B�?B�FB�RB�XB�^B�XB�XB�jB�qB�qB�qB�qBĜBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�)B�;B�HB�TB�mB�B�B��B��B	B		7B	{B	�B	�B	!�B	%�B	,B	/B	33B	49B	6FB	6FB	7LB	:^B	=qB	?}B	A�B	C�B	G�B	J�B	M�B	O�B	P�B	R�B	VB	ZB	[#B	]/B	`BB	bNB	bNB	cTB	cTB	cTB	bNB	dZB	jB	n�B	q�B	s�B	t�B	t�B	u�B	x�B	{�B	|�B	}�B	�B	�=B	�PB	�PB	�VB	�\B	�hB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�?B	�XB	�dB	�qB	�}B	��B	��B	ÖB	ƨB	ƨB	ɺB	ɺB	ȴB	ǮB	ɺB	��B	��B	��B	��B	�
B	�)B	�;B	�;B	�BB	�BB	�NB	�TB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191756                              AO  ARCAADJP                                                                    20181005191756    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191756  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191756  QCF$                G�O�G�O�G�O�8000            