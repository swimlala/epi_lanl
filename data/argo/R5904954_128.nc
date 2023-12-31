CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:17Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191717  20181005191717  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d{��1   @��e���@4�z�G��dlbM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  BffB  B   B)33B.��B7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C	�fC  C�fC  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*�C,  C.  C0  C1�fC3�fC5�fC7�fC9�fC;�fC>  C@�CB  CC�fCF  CH�CJ  CK�fCN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp�Cr�Ct�Cv  Cw�fCz  C|  C~  C�  C��3C��3C��fC�  C�  C�  C��C��C�  C�  C��C�  C��3C��C�  C��fC�  C�  C��3C��3C��3C�  C��3C�  C�  C��3C�  C�  C��C�  C��3C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C��3C��fC��fC��3C��3C��C��3C�  C�  C��C�  C��3C�  C��C��C��C��3C�  C��3C��C��3C�  C�  C�  C��C��C��C��C��3C��3C��3C�  C��C��C�  C��3C�  C��C��C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C��C��C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C��C�  C��3C��3C��3C�  C��C��C�  C��3C�  D   D � D  D�fD  D� D  Dy�D��Dy�D  D� DfD� D��Dy�D  Dy�D��D	� D	��D
� DfD� D��D� DfD�fDfD�fD  Dy�D��D� D  D� D  D� DfDy�D  D�fDfD� D  D� D  D� D  D�fDfD� D  D� D��D� D  D� DfD� D��Dy�D��Dy�D   D �fD!fD!�fD"fD"�fD#fD#� D$  D$� D%  D%� D%��D&y�D&��D'y�D(  D(� D)  D)� D)��D*� D+fD+y�D+��D,y�D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D4��D5� D6fD6�fD7  D7y�D8  D8� D9  D9�fD:  D:y�D:��D;� D<fD<�fD=  D=y�D=��D>�fD?fD?�fD@  D@� DAfDA�fDBfDB�fDCfDC�fDD  DD� DE  DE�fDFfDF�fDG  DG� DG��DH�fDIfDI�fDJ  DJ� DK  DK�fDL�DL�fDM  DMy�DN  DN�fDOfDO�fDPfDP�fDQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DV��DW� DXfDX�fDY  DYy�DZ  DZ� D[fD[� D\fD\�fD]fD]�fD^fD^�fD_  D_� D`  D`y�D`��Da� DbfDb�fDcfDc�fDd  Dd� De  De� Df  Df� Dg  Dgs3Dg��Dh� DifDi��DjfDj� Dk  Dk�fDl  Dly�Dm  Dm�fDn  Dn� Do  Dos3Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Ds� DtfDt� DufDu� Du��Dv� DwfDw� Dw� Dy��D�6�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��B�\B	(�B�\B(�B!(�B*\)B/��B8B@BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{B�aHBԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�ǮB��{B��{B��{C J=CJ=CJ=CJ=CJ=C
0�CJ=C0�CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"c�C$J=C&J=C(J=C*c�C,J=C.J=C0J=C20�C40�C60�C80�C:0�C<0�C>J=C@c�CBJ=CD0�CFJ=CHc�CJJ=CL0�CNJ=CP0�CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=Cdc�CfJ=ChJ=CjJ=ClJ=CnJ=Cpc�Crc�Ctc�CvJ=Cx0�CzJ=C|J=C~J=C�%C�RC�RC��C�%C�%C�%C�1�C�1�C�%C�%C�1�C�%C�RC�1�C�%C��C�%C�%C�RC�RC�RC�%C�RC�%C�%C�RC�%C�%C�1�C�%C�RC�%C�RC�%C�%C�%C�%C�%C�RC�RC�%C�RC��C��C�RC�RC�1�C�RC�%C�%C�1�C�%C�RC�%C�1�C�1�C�1�C�RC�%C�RC�1�C�RC�%C�%C�%C�1�C�1�C�1�C�1�C�RC�RC�RC�%C�1�C�1�C�%C�RC�%C�1�C�1�C�%C�%C�RC�%C�1�C�%C�RC�RC�%C�%C�%C�1�C�1�C�1�C�1�C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�RC�%C�1�C�1�C�%C�RC�RC�RC�%C�1�C�1�C�%C�RC�%D �D ��D�D��D�D��D�D�)D)D�)D�D��D�D��D)D�)D�D�)D	)D	��D
)D
��D�D��D)D��D�D��D�D��D�D�)D)D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D)D�)D)D�)D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&)D&�)D')D'�)D(�D(��D)�D)��D*)D*��D+�D+�)D,)D,�)D-�D-��D.�D.��D/�D/��D0�D0��D1)D1��D2�D2��D3�D3��D4�D4��D5)D5��D6�D6��D7�D7�)D8�D8��D9�D9��D:�D:�)D;)D;��D<�D<��D=�D=�)D>)D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH)DH��DI�DI��DJ�DJ��DK�DK��DL\DL��DM�DM�)DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV�)DW)DW��DX�DX��DY�DY�)DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`�)Da)Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh)Dh��Di�Di�\Dj�Dj��Dk�Dk��Dl�Dl�)Dm�Dm��Dn�Dn��Do�Do��Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds��Dt�Dt��Du�Du��Dv)Dv��Dw�Dw��Dw�Dy�GD�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A�  A���A���A���A�A���A���A���A���Aۉ7A��`A�ȴA�9XA�A�A̰!A�5?A�oA�Q�A�K�A��`A�O�A�ffA�7LA���A��#A�~�A�9XA��RA�
=A�^5A�$�A�G�A�oA��A��hA��!A��-A�E�A��
A��A�;dA�1'A��^A���A�Q�A�`BA��9A���A���A��PA�ZA��A�A��A�jA�hsA��A�1'A��A��wA�A�  A���A�33A��A��RA��`A�A�A�p�A���A�{A�l�A~�yA{�mAy��Av�uAt��AshsAq�Ap^5Ao�Am�^Al�AkAh�HAf�DAeO�Ac��Aap�A]�A[\)AX=qAU�PAT��AR�AP�AO�7AN�AM��AL�AJAG�AE��AD��ACAB��AA�AA
=A?��A=O�A<I�A;t�A;�A:�+A:5?A9��A8��A8��A8jA7"�A6JA3�
A2��A2$�A0�jA0r�A/A-��A,�\A+�A+
=A*~�A(ȴA'`BA'"�A&��A&r�A&�A%��A%�A$ �A#x�A#"�A"ȴA!��A�A��A�DA(�A�
Ar�AS�A(�A�A��AG�A�A�#AoAAv�A��A�yA��A�!AA�A�A��A �A;dA�yAhsA
^5A	��A	C�A	A�A1A��A �A�A�
AA�^A�hA�DA�AS�A
=A v�@��w@��@�n�@�K�@��@�%@�!@�x�@���@�R@�v�@�1'@�R@�{@�`B@�A�@��H@��@�1'@��@�G�@�$�@�33@�&�@֗�@ԋD@�|�@�o@���@��`@�b@��@��@�;d@��H@ʇ+@�^5@ɉ7@Ǯ@��@�x�@�%@��@�O�@���@��@�o@�x�@���@�$�@��@��-@�G�@���@��w@�v�@�=q@�p�@��@��#@��@�$�@��@�j@��@��9@��D@���@�@��R@��^@��@�9X@�ƨ@�ȴ@�{@�x�@�=q@��T@�1'@��@�dZ@���@��@�ƨ@�|�@��@��@�O�@��D@�I�@�(�@��m@��@�v�@�5?@�{@�=q@�;d@�C�@�(�@��@���@��^@��\@�33@��!@�X@�j@���@���@�v�@�E�@�$�@���@�V@��/@��@��F@�l�@�33@�33@�@���@���@���@���@�~�@�n�@�n�@��\@�n�@�-@���@��D@�Z@�1@���@�|�@�\)@�o@�o@��@�+@�ȴ@���@���@�5?@��@�`B@���@��T@�@�-@�V@��+@���@���@��\@��\@���@���@���@��@��@��y@�^5@��-@�hs@�?}@�A�@�v�@�bN@�ƨ@�|�@�o@��!@�ȴ@���@��@��D@�%@��@�/@��^@���@�hs@�?}@��@��j@��D@�j@��@��u@�b@���@�S�@��@���@���@�-@���@�?}@�&�@�O�@�G�@�&�@��`@���@�Ĝ@�Ĝ@��@�1'@��@��;@��P@�C�@�@��\@�v�@�
=@�C�@�;d@�$�@���@���@���@���@���@��@�bN@�9X@�Z@��u@�  @��@�K�@��@��H@��y@���@��@��R@��!@�ȴ@���@�~�@�n�@�^5@��@�x�@�&�@�V@�%@��@��@�%@�`B@�G�@�7L@�Ĝ@� �@�S�@�+@�K�@��P@���@�\)@�S�@���@�dZ@���@��@�l�@�/@��j@��T@��@��@�O�@���@�bN@�A�@�I�@�r�@��@��@�t�@�+@��\@�M�@�M�@���@���@�`B@��`@�z�@~B[@i�>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A�  A���A���A���A�A���A���A���A���Aۉ7A��`A�ȴA�9XA�A�A̰!A�5?A�oA�Q�A�K�A��`A�O�A�ffA�7LA���A��#A�~�A�9XA��RA�
=A�^5A�$�A�G�A�oA��A��hA��!A��-A�E�A��
A��A�;dA�1'A��^A���A�Q�A�`BA��9A���A���A��PA�ZA��A�A��A�jA�hsA��A�1'A��A��wA�A�  A���A�33A��A��RA��`A�A�A�p�A���A�{A�l�A~�yA{�mAy��Av�uAt��AshsAq�Ap^5Ao�Am�^Al�AkAh�HAf�DAeO�Ac��Aap�A]�A[\)AX=qAU�PAT��AR�AP�AO�7AN�AM��AL�AJAG�AE��AD��ACAB��AA�AA
=A?��A=O�A<I�A;t�A;�A:�+A:5?A9��A8��A8��A8jA7"�A6JA3�
A2��A2$�A0�jA0r�A/A-��A,�\A+�A+
=A*~�A(ȴA'`BA'"�A&��A&r�A&�A%��A%�A$ �A#x�A#"�A"ȴA!��A�A��A�DA(�A�
Ar�AS�A(�A�A��AG�A�A�#AoAAv�A��A�yA��A�!AA�A�A��A �A;dA�yAhsA
^5A	��A	C�A	A�A1A��A �A�A�
AA�^A�hA�DA�AS�A
=A v�@��w@��@�n�@�K�@��@�%@�!@�x�@���@�R@�v�@�1'@�R@�{@�`B@�A�@��H@��@�1'@��@�G�@�$�@�33@�&�@֗�@ԋD@�|�@�o@���@��`@�b@��@��@�;d@��H@ʇ+@�^5@ɉ7@Ǯ@��@�x�@�%@��@�O�@���@��@�o@�x�@���@�$�@��@��-@�G�@���@��w@�v�@�=q@�p�@��@��#@��@�$�@��@�j@��@��9@��D@���@�@��R@��^@��@�9X@�ƨ@�ȴ@�{@�x�@�=q@��T@�1'@��@�dZ@���@��@�ƨ@�|�@��@��@�O�@��D@�I�@�(�@��m@��@�v�@�5?@�{@�=q@�;d@�C�@�(�@��@���@��^@��\@�33@��!@�X@�j@���@���@�v�@�E�@�$�@���@�V@��/@��@��F@�l�@�33@�33@�@���@���@���@���@�~�@�n�@�n�@��\@�n�@�-@���@��D@�Z@�1@���@�|�@�\)@�o@�o@��@�+@�ȴ@���@���@�5?@��@�`B@���@��T@�@�-@�V@��+@���@���@��\@��\@���@���@���@��@��@��y@�^5@��-@�hs@�?}@�A�@�v�@�bN@�ƨ@�|�@�o@��!@�ȴ@���@��@��D@�%@��@�/@��^@���@�hs@�?}@��@��j@��D@�j@��@��u@�b@���@�S�@��@���@���@�-@���@�?}@�&�@�O�@�G�@�&�@��`@���@�Ĝ@�Ĝ@��@�1'@��@��;@��P@�C�@�@��\@�v�@�
=@�C�@�;d@�$�@���@���@���@���@���@��@�bN@�9X@�Z@��u@�  @��@�K�@��@��H@��y@���@��@��R@��!@�ȴ@���@�~�@�n�@�^5@��@�x�@�&�@�V@�%@��@��@�%@�`B@�G�@�7L@�Ĝ@� �@�S�@�+@�K�@��P@���@�\)@�S�@���@�dZ@���@��@�l�@�/@��j@��T@��@��@�O�@���@�bN@�A�@�I�@�r�@��@��@�t�@�+@��\@�M�@�M�@���@���@�`B@��`@�z�@~B[@i�>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B'�B1'BD�B@�B6FB<jBN�B`BBe`Bm�Bp�Bt�B� B�+B�{B��B��B��B��B��B�B�-B�LB�qB��BƨBɺB��BǮBÖB�^B��B��B��B�VB�%Bx�B\)BT�BE�B5?B#�BbB  B�B�/B�wB��B�DBhsB?}B�BDB
�B
�B
��B
�qB
��B
�B
t�B
iyB
T�B
H�B
:^B
%�B
�B
B	��B	�B	�NB	�B	��B	ƨB	��B	�XB	�B	��B	�uB	�7B	}�B	hsB	YB	I�B	<jB	6FB	-B	#�B	�B	�B	uB	PB	B��B�B�sB�`B�BB�/B�B��B��BȴBŢBĜBÖBÖB��BB��B��B�jB�XB�^B�RB�LB�LB�?B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�uB�oB�oB�hB�bB�bB�\B�VB�DB�VB�hB�bB�VB�bB�PB�VB�7B�=B�PB�JB�DB�PB�JB�JB�DB�7B�7B�7B�=B�=B�=B�=B�=B�=B�7B�7B�=B�=B�=B�=B�=B�7B�+B�B� B{�Bz�Bx�Bw�Bv�Bu�Bw�Bw�Bw�Bw�Bx�Bw�Bt�Bp�Bl�BiyB^5BS�BVBVBaHBcTBe`Be`BdZBcTB^5B\)B^5B_;B`BBaHBcTBaHB^5BZB\)B]/BcTBcTBdZBgmBhsB�+B�hB�=B�B�B�B�B|�B}�B� B}�Bv�Bv�B�7B��B��B��B��B��B�'B�3B�9B�XB�^B�XB�LB�?B�9B�'B�B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�LB�^B�^B�^B�^B�qB��B��B��B�;B�TB�fB�B�B��B	B	%B		7B	DB	JB	VB	bB	hB	uB	�B	�B	�B	�B	�B	�B	 �B	$�B	,B	0!B	2-B	33B	33B	49B	5?B	9XB	=qB	?}B	A�B	G�B	H�B	I�B	I�B	I�B	H�B	J�B	J�B	K�B	L�B	O�B	P�B	R�B	S�B	XB	]/B	bNB	hsB	l�B	n�B	p�B	s�B	u�B	v�B	w�B	w�B	x�B	z�B	|�B	�B	�B	�B	�B	�B	� B	� B	� B	{�B	v�B	t�B	u�B	x�B	{�B	�B	�7B	�PB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�dB	�jB	�jB	�qB	�qB	�qB	�qB	�jB	�jB	�jB	�jB	�jB	�wB	�}B	�}B	�}B	��B	ÖB	ĜB	ĜB	ƨB	ƨB	ƨB	ŢB	ĜB	ĜB	ĜB	ŢB	ɺB	��B	��B	��B	ǮB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�HB	�TB	�ZB	�ZB	�TB	�NB	�BB	�HB	�NB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B
B
  B
+B

=B
DB

=B
	7B
1B
1B
	7B
DB

=B
	7B
	7B
1B
1B
1B
1B

=B
DB
	7B
+B
	7B
�B
[22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B'�B1'BD�B@�B6FB<jBN�B`BBe`Bm�Bp�Bt�B� B�+B�{B��B��B��B��B��B�B�-B�LB�qB��BƨBɺB��BǮBÖB�^B��B��B��B�VB�%Bx�B\)BT�BE�B5?B#�BbB  B�B�/B�wB��B�DBhsB?}B�BDB
�B
�B
��B
�qB
��B
�B
t�B
iyB
T�B
H�B
:^B
%�B
�B
B	��B	�B	�NB	�B	��B	ƨB	��B	�XB	�B	��B	�uB	�7B	}�B	hsB	YB	I�B	<jB	6FB	-B	#�B	�B	�B	uB	PB	B��B�B�sB�`B�BB�/B�B��B��BȴBŢBĜBÖBÖB��BB��B��B�jB�XB�^B�RB�LB�LB�?B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�uB�oB�oB�hB�bB�bB�\B�VB�DB�VB�hB�bB�VB�bB�PB�VB�7B�=B�PB�JB�DB�PB�JB�JB�DB�7B�7B�7B�=B�=B�=B�=B�=B�=B�7B�7B�=B�=B�=B�=B�=B�7B�+B�B� B{�Bz�Bx�Bw�Bv�Bu�Bw�Bw�Bw�Bw�Bx�Bw�Bt�Bp�Bl�BiyB^5BS�BVBVBaHBcTBe`Be`BdZBcTB^5B\)B^5B_;B`BBaHBcTBaHB^5BZB\)B]/BcTBcTBdZBgmBhsB�+B�hB�=B�B�B�B�B|�B}�B� B}�Bv�Bv�B�7B��B��B��B��B��B�'B�3B�9B�XB�^B�XB�LB�?B�9B�'B�B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�LB�^B�^B�^B�^B�qB��B��B��B�;B�TB�fB�B�B��B	B	%B		7B	DB	JB	VB	bB	hB	uB	�B	�B	�B	�B	�B	�B	 �B	$�B	,B	0!B	2-B	33B	33B	49B	5?B	9XB	=qB	?}B	A�B	G�B	H�B	I�B	I�B	I�B	H�B	J�B	J�B	K�B	L�B	O�B	P�B	R�B	S�B	XB	]/B	bNB	hsB	l�B	n�B	p�B	s�B	u�B	v�B	w�B	w�B	x�B	z�B	|�B	�B	�B	�B	�B	�B	� B	� B	� B	{�B	v�B	t�B	u�B	x�B	{�B	�B	�7B	�PB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�dB	�jB	�jB	�qB	�qB	�qB	�qB	�jB	�jB	�jB	�jB	�jB	�wB	�}B	�}B	�}B	��B	ÖB	ĜB	ĜB	ƨB	ƨB	ƨB	ŢB	ĜB	ĜB	ĜB	ŢB	ɺB	��B	��B	��B	ǮB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�HB	�TB	�ZB	�ZB	�TB	�NB	�BB	�HB	�NB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B
B
  B
+B

=B
DB

=B
	7B
1B
1B
	7B
DB

=B
	7B
	7B
1B
1B
1B
1B

=B
DB
	7B
+B
	7B
�B
[22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191717                              AO  ARCAADJP                                                                    20181005191717    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191717  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191717  QCF$                G�O�G�O�G�O�8000            