CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:42Z AOML 3.0 creation; 2016-08-07T21:36:30Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221342  20160807143630  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_021                   2C  D   APEX                            6531                            072314                          846 @�,�P���1   @�,��/�@1|(�\�d+I�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�ffB���B���B���B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg�fDhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dys3D�3D�@ D�vfD���D�� D�,�D��3D�� D�3D�P D�� D�� D�	�D�@ D�|�D�� D�  D�C3D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�\)B��\B���B���B���B�(�B�(�B�(�B�(�B�\)B�\)B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL.CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D��D�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#�D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@~�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�Dg�Dg��Dh�Dh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dt��DyxRD��D�B�D�x�D��\D��D�/\D���D���D��D�R�D���D��D�)D�B�D�\D�ҏD��D�E�D�)D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AЇ+AЋDAЗ�AЛ�AЙ�AЛ�AЛ�AП�AС�AС�AС�AУ�AУ�AХ�AХ�AХ�AЧ�AЧ�AЩ�AЩ�AЬAЬAЬAЮAЮAа!Aа!Aа!Aа!AЮAЮAЬAЬAЧ�AГuA�A�
=A��A�;dA���A�dZA�S�A�ffA��A�5?A��A� �A��A���A��7A��mA��9A�
=A��A�I�A�A�l�A�~�A��mA�=qA�1'A���A���A���A��
A�p�A��!A�{A��wA��;A�&�A��!A�ƨA��mA��FA��RA�t�A�dZA�=qA��A��;A��A��A�n�A�M�A��jA���A��HA�ĜA�hsA�A��A��A��A�l�A�l�A��yA���A��^A{��Ax�yAv��Aq�^An�Al�Ai��Ad��Ab  A`VA^�RA[��AX�9AV��AT-APVAP-AO�TAMO�AI?}AGdZADz�AC&�ABffAA�A?�;A>-A;`BA8��A6A�A5K�A4��A4M�A2{A1G�A0z�A/7LA-�wA,VA+��A*�9A*��A*�\A*z�A*I�A)�TA(�!A'"�A$n�A#�hA"�A!�A ~�AhsA��AXA�9A�#At�AC�A��AE�A��A7LA�9A�hA��A�;A+A��AXA�7A��A��A�jA$�A/A�\A�TAJA
v�A��A9XA7LA �AoA�/A1A�;AC�A��A�AK�A �/@���@���@�@���@���@��w@��@�O�@�n�@��@�P@�\@��@�@�&�@�9@�@�hs@�!@��@� �@�1@���@�b@�Ĝ@�O�@��@�7@�bN@��
@�-@ᙚ@���@��#@ᙚ@�Ĝ@�t�@��@��#@�&�@�z�@�(�@�  @�C�@�ff@٩�@�Z@�ƨ@��@�v�@�-@�@��/@�Ĝ@ӕ�@�v�@�~�@�$�@�@���@�X@���@��m@��@�x�@̛�@�(�@˶F@� �@��/@�V@��@��@��@���@̣�@̃@�9X@�b@�t�@�
=@��y@���@ʗ�@���@�Ĝ@�1'@ǅ@�ȴ@Ƈ+@�^5@�-@�@Ų-@�V@Ĭ@�9X@��m@�C�@���@¸R@�J@��-@�X@�O�@�?}@��@�%@��j@�A�@�K�@�
=@���@�v�@�^5@�=q@�@��7@���@�  @��@��D@�1@�t�@�+@�~�@��@���@�p�@�?}@�&�@��j@�1@���@�K�@�@���@�^5@�=q@��@���@�`B@�G�@���@���@�j@�1@��P@�l�@�S�@��@��H@��+@�{@�@��@�&�@���@��@�9X@� �@�1@�  @��
@���@�t�@��@���@�n�@��-@�&�@���@�A�@��@���@��@�t�@�33@��@��@�v�@�-@��@���@�p�@��@���@��m@���@�C�@��!@�J@��h@�G�@�%@��9@��@�Q�@��m@��@�;d@���@�^5@�=q@�J@���@��^@���@�?}@���@�I�@�Q�@�j@�j@� �@��m@��
@��w@�t�@��@��@��!@��\@�^5@�{@���@���@�O�@��@��/@��@��@�b@��P@��@���@�ȴ@��\@��@�x�@�&�@��j@�A�@�  @��@�l�@�K�@��R@��+@�n�@�J@���@��@�@�`B@�/@��j@��@�1'@��@�ƨ@�S�@�o@��@��H@��@��@�ȴ@��\@�-@�@���@�7L@���@���@�z�@�Q�@�A�@�9X@�1'@�(�@��@�b@���@��w@�|�@�+@�@��y@��@���@��R@��+@�J@���@��@��@���@�7L@z�@m?}@cdZ@[C�@T�D@M��@DZ@=�-@6�R@0��@(  @"M�@n�@p�@7L@ƨ@�`@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AЇ+AЋDAЗ�AЛ�AЙ�AЛ�AЛ�AП�AС�AС�AС�AУ�AУ�AХ�AХ�AХ�AЧ�AЧ�AЩ�AЩ�AЬAЬAЬAЮAЮAа!Aа!Aа!Aа!AЮAЮAЬAЬAЧ�AГuA�A�
=A��A�;dA���A�dZA�S�A�ffA��A�5?A��A� �A��A���A��7A��mA��9A�
=A��A�I�A�A�l�A�~�A��mA�=qA�1'A���A���A���A��
A�p�A��!A�{A��wA��;A�&�A��!A�ƨA��mA��FA��RA�t�A�dZA�=qA��A��;A��A��A�n�A�M�A��jA���A��HA�ĜA�hsA�A��A��A��A�l�A�l�A��yA���A��^A{��Ax�yAv��Aq�^An�Al�Ai��Ad��Ab  A`VA^�RA[��AX�9AV��AT-APVAP-AO�TAMO�AI?}AGdZADz�AC&�ABffAA�A?�;A>-A;`BA8��A6A�A5K�A4��A4M�A2{A1G�A0z�A/7LA-�wA,VA+��A*�9A*��A*�\A*z�A*I�A)�TA(�!A'"�A$n�A#�hA"�A!�A ~�AhsA��AXA�9A�#At�AC�A��AE�A��A7LA�9A�hA��A�;A+A��AXA�7A��A��A�jA$�A/A�\A�TAJA
v�A��A9XA7LA �AoA�/A1A�;AC�A��A�AK�A �/@���@���@�@���@���@��w@��@�O�@�n�@��@�P@�\@��@�@�&�@�9@�@�hs@�!@��@� �@�1@���@�b@�Ĝ@�O�@��@�7@�bN@��
@�-@ᙚ@���@��#@ᙚ@�Ĝ@�t�@��@��#@�&�@�z�@�(�@�  @�C�@�ff@٩�@�Z@�ƨ@��@�v�@�-@�@��/@�Ĝ@ӕ�@�v�@�~�@�$�@�@���@�X@���@��m@��@�x�@̛�@�(�@˶F@� �@��/@�V@��@��@��@���@̣�@̃@�9X@�b@�t�@�
=@��y@���@ʗ�@���@�Ĝ@�1'@ǅ@�ȴ@Ƈ+@�^5@�-@�@Ų-@�V@Ĭ@�9X@��m@�C�@���@¸R@�J@��-@�X@�O�@�?}@��@�%@��j@�A�@�K�@�
=@���@�v�@�^5@�=q@�@��7@���@�  @��@��D@�1@�t�@�+@�~�@��@���@�p�@�?}@�&�@��j@�1@���@�K�@�@���@�^5@�=q@��@���@�`B@�G�@���@���@�j@�1@��P@�l�@�S�@��@��H@��+@�{@�@��@�&�@���@��@�9X@� �@�1@�  @��
@���@�t�@��@���@�n�@��-@�&�@���@�A�@��@���@��@�t�@�33@��@��@�v�@�-@��@���@�p�@��@���@��m@���@�C�@��!@�J@��h@�G�@�%@��9@��@�Q�@��m@��@�;d@���@�^5@�=q@�J@���@��^@���@�?}@���@�I�@�Q�@�j@�j@� �@��m@��
@��w@�t�@��@��@��!@��\@�^5@�{@���@���@�O�@��@��/@��@��@�b@��P@��@���@�ȴ@��\@��@�x�@�&�@��j@�A�@�  @��@�l�@�K�@��R@��+@�n�@�J@���@��@�@�`B@�/@��j@��@�1'@��@�ƨ@�S�@�o@��@��H@��@��@�ȴ@��\@�-@�@���@�7L@���@���@�z�@�Q�@�A�@�9X@�1'@�(�@��@�b@���@��w@�|�@�+@�@��y@��@���@��R@��+@�J@���G�O�@��@���@�7L@z�@m?}@cdZ@[C�@T�D@M��@DZ@=�-@6�R@0��@(  @"M�@n�@p�@7L@ƨ@�`@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
t�B
x�B
�+B
��B
��B
�B
��BBuB�B�B%�B<jB5?B2-Bo�B�B��B��BffBYB�yB2-BgmBv�Bv�B�DB�{B��B��B�B�'B��B��B�uB��B�hBs�Bq�Bm�BgmBffBhsBgmBgmBe`B`BBC�B"�BJB�B�
B�?B�%B?}B
=B
��B
�#B
��B
�+B
x�B
l�B
YB
E�B
&�B
DB	�NB	ƨB	�9B	�hB	y�B	hsB	S�B	<jB	1'B	$�B	�B	DB��B��B�B�B�B�fB�BB�5B�B�
B�B��B��B�B�
B�B�B�#B�B�B�B�#B�/B�BB�sB�B�B�B�B�B�B�B�B�B�sB�TB�#B�/B�/B�/B�/B�HB�`B�sB�fB�mB�fB�fB�`B�`B�mB�mB�fB�mB�ZB�BB�5B�B��B��BɺBƨBÖB��B��B�}BĜB�NB�NB�/B�NB�HB�/B�;B�B�sB�yB�B�yB�yB�`B�BB�B��B�B�B�5B�NB�`B�yB�TB�TB�NB�NB�HB�BB�;B�5B�#B�B��B��B�B�B�B�ZB�B��B	B	B	B	
=B	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	&�B	'�B	'�B	'�B	'�B	'�B	(�B	)�B	-B	+B	,B	1'B	:^B	?}B	?}B	@�B	C�B	B�B	C�B	D�B	C�B	D�B	F�B	K�B	S�B	XB	YB	ZB	ZB	]/B	^5B	`BB	cTB	e`B	hsB	k�B	l�B	l�B	m�B	o�B	q�B	r�B	u�B	x�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�3B	�3B	�?B	�?B	�FB	�RB	�^B	�dB	�dB	�jB	�wB	�}B	�}B	��B	��B	B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�
B	�
B	�B	�#B	�/B	�5B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
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
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
bB
\B
�B
�B
�B
$�B
,B
0!B
6FB
=qB
D�B
K�B
Q�B
W
B
]/B
bNB
iyB
n�B
r�B
w�B
|�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
t�B
x�B
�*B
��B
��B
�B
��BBpB�B�B%�B<eB5:B2(Bo�B�B��B��Bf^BYB�sB2%BgfBv�Bv�B�9B�uB��B��B��B�"B��B��B�lB��B�aBs�Bq�Bm�BgdBfcBhjBgdBgeBeYB`=BC�B"�BCB�B�B�9B�B?zB
6B
��B
�B
��B
�'B
x�B
l�B
YB
E�B
&�B
FB	�MB	ƧB	�<B	�kB	y�B	hzB	S�B	<pB	1-B	$�B	�B	MB��B��B�B�B�B�pB�MB�=B�"B�B�B�B�	B�B�B� B�)B�+B� B�B�B�+B�9B�JB�yB�B�B�B�B�B�B�B�B�B�zB�]B�,B�7B�9B�7B�9B�NB�gB�{B�mB�tB�nB�nB�hB�fB�tB�vB�nB�tB�aB�IB�=B�&B��B��B��BƮBßB��B��B��BĤB�VB�VB�6B�WB�NB�7B�EB�B�{B�B�B�B�B�fB�IB�B��B�B�%B�=B�UB�eB�B�\B�^B�XB�XB�OB�IB�CB�<B�+B�B� B��B�B�B�&B�`B�B��B	B	B	B	
BB	bB	uB	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	&�B	'�B	'�B	'�B	'�B	'�B	(�B	*B	-B	+B	,B	1*B	:aB	?�B	?�B	@�B	C�B	B�B	C�B	D�B	C�B	D�B	F�B	K�B	S�B	XB	YB	Z B	Z!B	]/B	^6B	`DB	cUB	e`B	hvB	k�B	l�B	l�B	m�B	o�B	q�B	r�B	u�B	x�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�)B	�=B	�NB	�pB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�0B	�2B	�3B	�3B	�;B	�?B	�CB	�QB	�]B	�bB	�dB	�iB	�uB	�|B	�|B	��B	��B	B	B	B	ÓB	ĚB	şB	ŠB	ƦB	ǬB	ǬB	ǬB	ɵB	ɸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�	B	�B	�B	� B	�,B	�1B	�-B	�-B	�,B	�,B	�-B	�/B	�8B	�?B	�>B	�EB	�JB	�JB	�JB	�JB	�QB	�YB	�XB	�dB	�aB	�jB	�jB	�kB	�rB	�qB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
 B
B
B
B
B
	B
B
B
B
B
B
B
"B
#B
!B
!B
!B
B
!B
"B
 B
(B
)B
.B
,B
	3B
	4B
	4B
	1B
	1B

9B
AG�O�B
ZB
�B
�B
�B
$�B
,B
0B
6AB
=lB
D�B
K�B
Q�B
WB
]'B
bIB
isB
n�B
r�B
w�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.08 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436302016080714363020160807143630  AO  ARCAADJP                                                                    20150226221342    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221342  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221342  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143630  IP                  G�O�G�O�G�O�                