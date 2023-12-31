CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:10Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190510  20181005190510  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ׯ�6�,�1   @ׯ��l)�@2�l�C���c~ȴ9X1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   AA��Aa��A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B���B�  B�  B�  B�33B�  B�  B�  B�33B�33B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���C  C  C  C  C
�CL�C��C�fC�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C�  D fD �fD  D� D  Dy�D��D� DfD� D  D�fDfD� D  D� D  D� D	  D	� D
fD
�fDfD�fDfD�fD  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  Dy�D��D� D  D� D  D�fD   D � D!  D!�fD"fD"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,y�D-  D-� D-��D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3y�D4  D4�fD5fD5� D6  D6y�D6��D7y�D8  D8� D9  D9� D:fD:�fD;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF�fDGfDG�fDH  DH� DI  DI� DJfDJ�fDKfDK� DK��DLy�DL��DMy�DN  DN� DOfDO� DO��DP� DQfDQ� DR  DR� DSfDS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D]��D^� D_  D_�fD`  D`y�Da  Day�Db  Db� Db��Dc� Dd  Dd� Dd��Dey�Df  Df� DgfDg�fDh  Dh� Dh��Di� DjfDj� Dk  Dk�fDl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dr� Ds  Dsy�Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� DwٚDy�\D�O�D�e�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�{A
=A#
=AD��Ad��A��A��A��RA��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�.B�.B�aHB�aHB�aHB��{B�aHB�aHB�aHB��{B��{B��{B��{B�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�{B�aHB�aHB�aHB�aHB�aHC 
C0�C0�C0�C0�C
J>C}qC�qC
C
C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,J>C.0�C00�C20�C40�C60�C80�C:J>C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf
Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�%C�RC�%C�RC�RC�RC�RC�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�%C�RC�RC�RC��C�RC�%C�RC�RC�RC�RC�RC�%C�%C�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C��C��C�RC��C��C��C��C��C��C��C�RC�%C�RC�RC�RD �D ��D)D�)D)D��D�D�)D�D�)D)D��D�D�)D)D�)D)D�)D	)D	�)D
�D
��D�D��D�D��D)D��D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D��D)D�)D)D��D�D�)D)D�)D)D��D )D �)D!)D!��D"�D"��D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)��D*)D*�)D+)D+�)D,)D,��D-)D-�)D.�D.�)D/)D/��D0)D0�)D1)D1�)D2)D2�)D3)D3��D4)D4��D5�D5�)D6)D6��D7�D7��D8)D8�)D9)D9�)D:�D:��D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?�D?�)D@)D@�)DA)DA�)DB)DB��DC)DC�)DD)DD�)DE)DE�)DF)DF��DG�DG��DH)DH�)DI)DI�)DJ�DJ��DK�DK�)DL�DL��DM�DM��DN)DN�)DO�DO�)DP�DP�)DQ�DQ�)DR)DR�)DS�DS�)DT)DT�)DU�DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]��D^�D^�)D_)D_��D`)D`��Da)Da��Db)Db�)Dc�Dc�)Dd)Dd�)De�De��Df)Df�)Dg�Dg��Dh)Dh�)Di�Di�)Dj�Dj�)Dk)Dk��Dl)Dl�)Dm�Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq��Dr�Dr�)Ds)Ds��Dt)Dt��Du)Du�)Dv)Dv�)Dw)Dw�)Dw��Dy��D�U�D�k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�ffA�jA�n�A�G�A��AУ�A�"�A�+A�M�AЋDAЛ�AЛ�A�l�A�K�A��A���A��mA���A��;A���AϺ^A���Aϕ�A�ffA�Q�A�Q�A�p�A�1'A��mAήAΓuAΏ\A�r�A�=qA��A͍PA̟�A�7LA��A�"�A���A�Q�Aʴ9Aʟ�A�?}A�33A�ĜA�`BAɃA��Aț�A�`BA�M�AƬA��AŅA�1'A�l�Aå�A�^5A�dZA�K�A�A��A�dZA�33A���A�dZA���A���A�ZA���A��-A�
=A���A�
=A���A�(�A�  A��wA��hA�5?A��PA�A�r�A���A�ȴA���A�%A���A�VA� �A��7A��mA��/A�ƨA�A�r�A��PA�+A���A�bA�dZA���A�/A�1A�A�bNA�ȴA��A��mA�9XA��!A���A�%A~��A}��A}�-A}�FA}��A|�yA{ƨAz�Az�\Ay�^Ax��Aw��Au��AtffAt  ArĜAm/Aj{Ahn�Ad��Ac+A`9XA\5?AZZAY;dATQ�AQ�PAO��AL�`AJ��AI��AFA�AA�A@A�A?hsA>-A=�-A=VA;t�A7�FA5l�A5oA4n�A3O�A01'A/%A.��A-�A,A�A*�A(�A'�FA&��A&ĜA&��A%�mA$�DA$1A#C�A!`BA�AK�AVAS�A(�A�wA9XAG�AC�AdZA+A�A\)AĜAJAA\)A�uAAVA	hsAA�mA�9A�\AG�AA �A Z@�+@���@�o@��-@��D@�Z@�1'@�dZ@���@���@�ȴ@��@�G�@���@��@�F@�@�~�@�$�@��#@�p�@�V@�Q�@�l�@�!@�E�@�@�|�@�J@�@��D@�v�@�V@��@���@�G�@�Q�@ׅ@�@�M�@ա�@��`@��;@�\)@�=q@љ�@���@�j@�b@��
@϶F@�=q@͙�@��@� �@�  @�dZ@�@���@��
@ǝ�@�ȴ@ŉ7@���@Ĭ@���@�+@�E�@���@��h@��@�p�@�`B@�7L@���@��j@��@�9X@�  @��@��@��;@��;@�  @�1'@�j@�Ĝ@��/@���@�%@�&�@�?}@�Ĝ@��@��;@���@��+@�=q@��T@��@�b@�(�@�;d@�~�@�M�@��^@���@�%@�t�@�o@�@�5?@�Ĝ@��P@�
=@�"�@�\)@���@���@��@�@�;d@�C�@�S�@�t�@��@��@�;d@�
=@�@�/@�33@���@��7@��@���@��D@�r�@�j@�9X@�1@��@�C�@���@�$�@��T@��h@�7L@��@���@��`@��D@�j@�9X@� �@�1@��@��;@��P@��@��\@�^5@�J@��@��T@���@���@�@��@�%@�Q�@��F@�ff@��^@��j@���@���@���@��m@���@�Q�@��D@���@� �@���@�n�@�{@�X@�A�@��@��@�|�@�l�@��R@���@�V@���@�r�@�bN@�bN@�Z@�A�@�(�@���@�S�@�C�@�33@�33@�33@�33@�t�@�1@���@��P@�\)@�S�@�K�@�@��@��y@���@�5?@��@�@��7@��`@��u@�Z@��w@�|�@�l�@�33@���@���@�E�@�@�@�7L@�Ĝ@�bN@�I�@�  @��w@�|�@�;d@�"�@�o@��y@��!@��\@��\@�v�@�ff@�n�@�V@�M�@�E�@�5?@�-@�-@�$�@�J@�@���@��@��T@���@��7@�G�@��@��/@�z�@�j@�Q�@�(�@�  @��@�v�@�V@�5?@���@���@��h@��@���@�Ĝ@��9@���@�bN@� �@�c@
=@l�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ffA�ffA�jA�n�A�G�A��AУ�A�"�A�+A�M�AЋDAЛ�AЛ�A�l�A�K�A��A���A��mA���A��;A���AϺ^A���Aϕ�A�ffA�Q�A�Q�A�p�A�1'A��mAήAΓuAΏ\A�r�A�=qA��A͍PA̟�A�7LA��A�"�A���A�Q�Aʴ9Aʟ�A�?}A�33A�ĜA�`BAɃA��Aț�A�`BA�M�AƬA��AŅA�1'A�l�Aå�A�^5A�dZA�K�A�A��A�dZA�33A���A�dZA���A���A�ZA���A��-A�
=A���A�
=A���A�(�A�  A��wA��hA�5?A��PA�A�r�A���A�ȴA���A�%A���A�VA� �A��7A��mA��/A�ƨA�A�r�A��PA�+A���A�bA�dZA���A�/A�1A�A�bNA�ȴA��A��mA�9XA��!A���A�%A~��A}��A}�-A}�FA}��A|�yA{ƨAz�Az�\Ay�^Ax��Aw��Au��AtffAt  ArĜAm/Aj{Ahn�Ad��Ac+A`9XA\5?AZZAY;dATQ�AQ�PAO��AL�`AJ��AI��AFA�AA�A@A�A?hsA>-A=�-A=VA;t�A7�FA5l�A5oA4n�A3O�A01'A/%A.��A-�A,A�A*�A(�A'�FA&��A&ĜA&��A%�mA$�DA$1A#C�A!`BA�AK�AVAS�A(�A�wA9XAG�AC�AdZA+A�A\)AĜAJAA\)A�uAAVA	hsAA�mA�9A�\AG�AA �A Z@�+@���@�o@��-@��D@�Z@�1'@�dZ@���@���@�ȴ@��@�G�@���@��@�F@�@�~�@�$�@��#@�p�@�V@�Q�@�l�@�!@�E�@�@�|�@�J@�@��D@�v�@�V@��@���@�G�@�Q�@ׅ@�@�M�@ա�@��`@��;@�\)@�=q@љ�@���@�j@�b@��
@϶F@�=q@͙�@��@� �@�  @�dZ@�@���@��
@ǝ�@�ȴ@ŉ7@���@Ĭ@���@�+@�E�@���@��h@��@�p�@�`B@�7L@���@��j@��@�9X@�  @��@��@��;@��;@�  @�1'@�j@�Ĝ@��/@���@�%@�&�@�?}@�Ĝ@��@��;@���@��+@�=q@��T@��@�b@�(�@�;d@�~�@�M�@��^@���@�%@�t�@�o@�@�5?@�Ĝ@��P@�
=@�"�@�\)@���@���@��@�@�;d@�C�@�S�@�t�@��@��@�;d@�
=@�@�/@�33@���@��7@��@���@��D@�r�@�j@�9X@�1@��@�C�@���@�$�@��T@��h@�7L@��@���@��`@��D@�j@�9X@� �@�1@��@��;@��P@��@��\@�^5@�J@��@��T@���@���@�@��@�%@�Q�@��F@�ff@��^@��j@���@���@���@��m@���@�Q�@��D@���@� �@���@�n�@�{@�X@�A�@��@��@�|�@�l�@��R@���@�V@���@�r�@�bN@�bN@�Z@�A�@�(�@���@�S�@�C�@�33@�33@�33@�33@�t�@�1@���@��P@�\)@�S�@�K�@�@��@��y@���@�5?@��@�@��7@��`@��u@�Z@��w@�|�@�l�@�33@���@���@�E�@�@�@�7L@�Ĝ@�bN@�I�@�  @��w@�|�@�;d@�"�@�o@��y@��!@��\@��\@�v�@�ff@�n�@�V@�M�@�E�@�5?@�-@�-@�$�@�J@�@���@��@��T@���@��7@�G�@��@��/@�z�@�j@�Q�@�(�@�  @��@�v�@�V@�5?@���@���@��h@��@���@�Ĝ@��9@���@�bN@� �@�c@
=@l�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�)B	�#B	�B	�B	�B	�B	�
B	�mB	��B
bB
!�B
%�B
(�B
'�B
$�B
�B
�B
�B
�B
�B
!�B
$�B
,B
$�B
�B
�B
�B
&�B
&�B
+B
49B
:^B
;dB
@�B
N�B
M�B
;dB
#�B
%�B
R�B
{�B
�1B
~�B
x�B
�+B
��B
�B
�BuB.BA�BL�BW
Bq�B� B�bB��B��B��B�wB�;B�B�B��BB
=BVB\BB%�BS�B^5BiyBk�BW
BG�BB�B>wB9XB7LB5?B33B/B#�B�B�BhB1B��B��B�B�BB��B��B�B�B�B��Bw�B`BB[#BR�BJ�B@�B8RB+B�B
��B
�B
��B
l�B
W
B
F�B
+B
\B
B	�sB	�sB	�sB	�mB	�B	�B	�`B	�;B	�)B	�B	��B	ǮB	�dB	�?B	�-B	��B	x�B	XB	D�B	0!B	(�B	�B	VB	%B	  B��B�B�sB�TB�BB�/B�
B��B��B��B��BȴBŢB��B�^B�XB�LB�?B�'B�3B�jB�dB�XB�LB�jB�RB�FB��B�TB�fB�mB�`B�NB�NB�/B��B��B��BƨB�}B�FB�9B�FBÖB�B�NB�NB�HB�mB�B�ZB��B��B��BĜB�XB�?B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�-B�-B�3B�FB�RB�XB�dB�jB�qB�}B�}BĜBǮB��B��B��B��B��B��B��B�
B�/B�NB�`B�B�B�B�B�B�B��B��B	B	B	1B	PB	PB	VB	VB	VB	\B	{B	�B	�B	!�B	(�B	)�B	)�B	,B	/B	1'B	49B	6FB	;dB	@�B	F�B	J�B	N�B	VB	[#B	_;B	aHB	e`B	l�B	n�B	n�B	n�B	s�B	y�B	w�B	w�B	{�B	}�B	�B	�B	~�B	}�B	�B	|�B	w�B	s�B	r�B	u�B	|�B	�B	�B	�1B	�=B	�PB	�PB	�VB	�\B	�bB	�bB	�hB	�hB	�VB	�JB	�B	�1B	�1B	�7B	�DB	�DB	�JB	�JB	�PB	�\B	�bB	�hB	�hB	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�-B	�3B	�3B	�9B	�3B	�3B	�9B	�9B	�9B	�9B	�9B	�?B	�RB	�^B	�qB	�}B	��B	ÖB	ĜB	ĜB	ǮB	ǮB	ȴB	��B	��B	��B	��B	ȴB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�HB	�NB	�NB	�NB	�TB	�`B	�`B	�fB	�fB	�`B	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
	7B
1B
+B
1B
1B
	7B
	7B
	7B
JB
JB
PB
PB
PB
VB
bB
vB
CB
'�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	�)B	�#B	�B	�B	�B	�B	�
B	�mB	��B
bB
!�B
%�B
(�B
'�B
$�B
�B
�B
�B
�B
�B
!�B
$�B
,B
$�B
�B
�B
�B
&�B
&�B
+B
49B
:^B
;dB
@�B
N�B
M�B
;dB
#�B
%�B
R�B
{�B
�1B
~�B
x�B
�+B
��B
�B
�BuB.BA�BL�BW
Bq�B� B�bB��B��B��B�wB�;B�B�B��BB
=BVB\BB%�BS�B^5BiyBk�BW
BG�BB�B>wB9XB7LB5?B33B/B#�B�B�BhB1B��B��B�B�BB��B��B�B�B�B��Bw�B`BB[#BR�BJ�B@�B8RB+B�B
��B
�B
��B
l�B
W
B
F�B
+B
\B
B	�sB	�sB	�sB	�mB	�B	�B	�`B	�;B	�)B	�B	��B	ǮB	�dB	�?B	�-B	��B	x�B	XB	D�B	0!B	(�B	�B	VB	%B	  B��B�B�sB�TB�BB�/B�
B��B��B��B��BȴBŢB��B�^B�XB�LB�?B�'B�3B�jB�dB�XB�LB�jB�RB�FB��B�TB�fB�mB�`B�NB�NB�/B��B��B��BƨB�}B�FB�9B�FBÖB�B�NB�NB�HB�mB�B�ZB��B��B��BĜB�XB�?B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�-B�-B�3B�FB�RB�XB�dB�jB�qB�}B�}BĜBǮB��B��B��B��B��B��B��B�
B�/B�NB�`B�B�B�B�B�B�B��B��B	B	B	1B	PB	PB	VB	VB	VB	\B	{B	�B	�B	!�B	(�B	)�B	)�B	,B	/B	1'B	49B	6FB	;dB	@�B	F�B	J�B	N�B	VB	[#B	_;B	aHB	e`B	l�B	n�B	n�B	n�B	s�B	y�B	w�B	w�B	{�B	}�B	�B	�B	~�B	}�B	�B	|�B	w�B	s�B	r�B	u�B	|�B	�B	�B	�1B	�=B	�PB	�PB	�VB	�\B	�bB	�bB	�hB	�hB	�VB	�JB	�B	�1B	�1B	�7B	�DB	�DB	�JB	�JB	�PB	�\B	�bB	�hB	�hB	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�-B	�3B	�3B	�9B	�3B	�3B	�9B	�9B	�9B	�9B	�9B	�?B	�RB	�^B	�qB	�}B	��B	ÖB	ĜB	ĜB	ǮB	ǮB	ȴB	��B	��B	��B	��B	ȴB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�HB	�NB	�NB	�NB	�TB	�`B	�`B	�fB	�fB	�`B	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
	7B
1B
+B
1B
1B
	7B
	7B
	7B
JB
JB
PB
PB
PB
VB
bB
vB
CB
'�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190510                              AO  ARCAADJP                                                                    20181005190510    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190510  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190510  QCF$                G�O�G�O�G�O�8000            