CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:08Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190608  20181005190608  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�Q�"1   @��jUUjF@2,������c�\(�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  B��B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  D   D � D  D� D  Dy�D��D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��Dy�D��Dy�D  D� D  D� D  D� D  D� D  D� DfD�fDfD�fD  Dy�D��D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"�fD#fD#� D$  D$� D$��D%y�D&  D&� D'  D'�fD(fD(� D(��D)� D*  D*� D+  D+� D,  D,y�D-  D-�fD.  D.� D/fD/� D0  D0� D1fD1� D2  D2y�D2��D3� D3��D4y�D4��D5y�D5��D6y�D6��D7y�D8  D8�fD9  D9y�D:  D:� D;  D;� D<  D<�fD=  D=� D>fD>� D>��D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJy�DK  DK� DL  DL�fDM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DSy�DT  DTy�DT��DU� DV  DV� DW  DW�fDXfDX� DY  DY�fDZ  DZy�D[  D[� D\  D\�fD]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Dby�Db��Dc� DdfDd� Dd��Dey�De��Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk�fDl  Dly�Dl��Dmy�Dn  Dny�Dn��Doy�Do��Dp� Dp��Dqy�Dq��Dr� Ds  Dsy�Ds��Dty�Du  Du�fDvfDv� Dv��Dwy�Dw��Dy��D�:=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @>�R@��\@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A�p�B �RBQ�BQ�B�B Q�B(Q�B0Q�B8Q�B@Q�BH�RBPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C	��C{C{C{C{C{C{C{C��C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD.CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`.Cb.Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C��pC�
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
=C��pC�
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
=C��pC��pC��pC�
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
=C��pC�
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
=C��pC�
=C�
=C�
=C�
=C�
=C��pC�
=C�
C�
=C�
=C��pC��pC��pC��pC��pC��pC�
=C�
C�
=C��pC�
=C�
=C�
=C�
=C�
C�
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
=C��pC�
=C�
=C��pC�
=C�
=D D �DD�DD~�D��D�DD�DD�DD�D��D�DD�D	D	�D
D
�DD�DD�DD�D��D~�D��D~�DD�DD�DD�DD�DD�D�D��D�D��DD~�D��D�D�D�DD�DD�DD�DD�DD�DD�D D �D!D!~�D"D"��D#�D#�D$D$�D$��D%~�D&D&�D'D'��D(�D(�D(��D)�D*D*�D+D+�D,D,~�D-D-��D.D.�D/�D/�D0D0�D1�D1�D2D2~�D2��D3�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D8D8��D9D9~�D:D:�D;D;�D<D<��D=D=�D>�D>�D>��D?�D@D@�DADA~�DBDB�DCDC�DDDD�DE�DE�DFDF�DGDG�DHDH�DIDI�DI��DJ~�DKDK�DLDL��DMDM~�DNDN�DODO�DPDP�DQDQ�DRDR�DR��DS~�DTDT~�DT��DU�DVDV�DWDW��DX�DX�DYDY��DZDZ~�D[D[�D\D\��D]D]�D^D^�D_�D_�D`D`�DaDa�DbDb~�Db��Dc�Dd�Dd�Dd��De~�De��Df�DgDg�DhDh�Dh��Di�DjDj�DkDk��DlDl~�Dl��Dm~�DnDn~�Dn��Do~�Do��Dp�Dp��Dq~�Dq��Dr�DsDs~�Ds��Dt~�DuDu��Dv�Dv�Dv��Dw~�Dw��Dy��D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��A���A���A��A���A���A���A���A��A��A��#A���AɋDA�&�A���A��yA�ȴA�ffA�r�A�+AǋDA��Aƥ�Aƛ�A��/A�bAđhA�Q�A��A�A�1'A§�A�r�A�C�A�&�A��A��A�\)A��A���A��9A��FA��!A���A��PA�I�A�  A��yA���A���A��DA�{A��jA��FA���A�v�A�ZA�7LA�
=A�
=A�%A��A���A�+A��;A�^5A��TA��/A�n�A�jA�|�A�?}A��RA���A���A�/A�&�A�A�C�A�{A�9XA�ĜA��A���A��wA���A�%A��#A�~�A��DA��A�9XA���A��+A�1A�\)A��HA���A�ĜA�VA��A�?}A�%A���A��A���A�\)A���A���A�dZA�?}A���A�n�A�Q�A���A��A~v�AvM�Ap��An��AmC�AlbAk�AgC�Ac
=A`�A]K�AY�
AXA�AV9XASp�AP��AM�AL1AI�
AG�AC��A?��A="�A:E�A9;dA5�A3dZA1;dA.r�A-dZA+�A*�9A*(�A)�A(��A'�wA&�yA$�RA �uAp�A�+AoA�-A\)A�mA�/A �AĜAA�A�^A~�A�#A�A�Av�AI�A�A��A��A�-A�FAl�A�A��AA�A�Av�A/AĜA�\AVA�^Ax�AS�AK�A+AXAl�At�A�A��A��A9XA
bNA��A�A&�A��A��A��A(�AO�A?}A33A ��@���@�7L@�ȴ@�-@��#@���@��@���@� �@�dZ@�n�@�x�@�&�@���@��
@���@�\@�^5@��@�-@�X@��@�(�@�R@��@�&�@��/@�@�7L@�(�@�F@�"�@���@�E�@�\@��y@���@�-@���@�V@�bN@�@��@�O�@�+@◍@�-@�j@߾w@�$�@ݙ�@��@ߥ�@߮@���@��T@�9X@۝�@�|�@�l�@��H@���@�O�@�/@���@ش9@ؓu@�bN@�b@��@�b@�\)@��@և+@�M�@Չ7@���@�Q�@���@���@Ӯ@�33@�n�@�`B@�r�@��@���@�%@˥�@�o@���@�n�@�V@�$�@���@��@���@�@�G�@��@��`@ȼj@ȋD@ȃ@�j@�(�@��;@ǅ@�33@�@��@Ɨ�@�ff@�5?@��T@Ų-@�V@Ĭ@���@Å@�"�@���@�=q@�/@���@�Z@�(�@�ƨ@�o@��H@��@��!@�=q@��^@���@��D@�A�@��
@���@���@�t�@�dZ@���@��\@�M�@�J@��T@�@�x�@�%@��`@�Ĝ@��@�1'@��;@�t�@�dZ@�dZ@�o@�
=@��H@��@��R@�ff@�{@���@���@��@�I�@�ƨ@�dZ@��@���@��^@��h@��7@��7@�hs@�O�@��@��/@��D@�j@�ƨ@���@�;d@��@�"�@�V@��@��/@�Z@�l�@�dZ@�K�@�
=@���@�K�@�;d@�|�@��@�ȴ@�5?@��T@��T@���@���@�/@�&�@��@��@��;@���@�C�@�"�@�
=@��H@�$�@��7@��@��9@�j@�1'@��@��w@���@�S�@���@�V@�@��^@�G�@��/@�z�@�A�@�1@��@��P@�|�@�t�@�33@���@�ȴ@�~�@��@���@���@�`B@���@�9X@��@��T@���@��7@�hs@��@��`@�Ĝ@�b@�ƨ@���@�+@��@��\@�V@�@���@�p�@�p�@�`B@�G�@�G�@��@��@���@�r�@���@�C�@�
=@��@���@���@��y@�l"@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��
A��A���A���A��A���A���A���A���A��A��A��#A���AɋDA�&�A���A��yA�ȴA�ffA�r�A�+AǋDA��Aƥ�Aƛ�A��/A�bAđhA�Q�A��A�A�1'A§�A�r�A�C�A�&�A��A��A�\)A��A���A��9A��FA��!A���A��PA�I�A�  A��yA���A���A��DA�{A��jA��FA���A�v�A�ZA�7LA�
=A�
=A�%A��A���A�+A��;A�^5A��TA��/A�n�A�jA�|�A�?}A��RA���A���A�/A�&�A�A�C�A�{A�9XA�ĜA��A���A��wA���A�%A��#A�~�A��DA��A�9XA���A��+A�1A�\)A��HA���A�ĜA�VA��A�?}A�%A���A��A���A�\)A���A���A�dZA�?}A���A�n�A�Q�A���A��A~v�AvM�Ap��An��AmC�AlbAk�AgC�Ac
=A`�A]K�AY�
AXA�AV9XASp�AP��AM�AL1AI�
AG�AC��A?��A="�A:E�A9;dA5�A3dZA1;dA.r�A-dZA+�A*�9A*(�A)�A(��A'�wA&�yA$�RA �uAp�A�+AoA�-A\)A�mA�/A �AĜAA�A�^A~�A�#A�A�Av�AI�A�A��A��A�-A�FAl�A�A��AA�A�Av�A/AĜA�\AVA�^Ax�AS�AK�A+AXAl�At�A�A��A��A9XA
bNA��A�A&�A��A��A��A(�AO�A?}A33A ��@���@�7L@�ȴ@�-@��#@���@��@���@� �@�dZ@�n�@�x�@�&�@���@��
@���@�\@�^5@��@�-@�X@��@�(�@�R@��@�&�@��/@�@�7L@�(�@�F@�"�@���@�E�@�\@��y@���@�-@���@�V@�bN@�@��@�O�@�+@◍@�-@�j@߾w@�$�@ݙ�@��@ߥ�@߮@���@��T@�9X@۝�@�|�@�l�@��H@���@�O�@�/@���@ش9@ؓu@�bN@�b@��@�b@�\)@��@և+@�M�@Չ7@���@�Q�@���@���@Ӯ@�33@�n�@�`B@�r�@��@���@�%@˥�@�o@���@�n�@�V@�$�@���@��@���@�@�G�@��@��`@ȼj@ȋD@ȃ@�j@�(�@��;@ǅ@�33@�@��@Ɨ�@�ff@�5?@��T@Ų-@�V@Ĭ@���@Å@�"�@���@�=q@�/@���@�Z@�(�@�ƨ@�o@��H@��@��!@�=q@��^@���@��D@�A�@��
@���@���@�t�@�dZ@���@��\@�M�@�J@��T@�@�x�@�%@��`@�Ĝ@��@�1'@��;@�t�@�dZ@�dZ@�o@�
=@��H@��@��R@�ff@�{@���@���@��@�I�@�ƨ@�dZ@��@���@��^@��h@��7@��7@�hs@�O�@��@��/@��D@�j@�ƨ@���@�;d@��@�"�@�V@��@��/@�Z@�l�@�dZ@�K�@�
=@���@�K�@�;d@�|�@��@�ȴ@�5?@��T@��T@���@���@�/@�&�@��@��@��;@���@�C�@�"�@�
=@��H@�$�@��7@��@��9@�j@�1'@��@��w@���@�S�@���@�V@�@��^@�G�@��/@�z�@�A�@�1@��@��P@�|�@�t�@�33@���@�ȴ@�~�@��@���@���@�`B@���@�9X@��@��T@���@��7@�hs@��@��`@�Ĝ@�b@�ƨ@���@�+@��@��\@�V@�@���@�p�@�p�@�`B@�G�@�G�@��@��@���@�r�@���@�C�@�
=@��@���@���@��y@�l"@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB��B�B�5B�yB	=qB
N�B
�B
�!B
��B
�B
��BBJBJBbB�B�B�B�B�B �B'�B,B(�B)�B+B/B6FB;dB:^BC�BF�BA�B>wBA�BF�BL�B[#Bq�B�B�{B��B�B�'B�9B�}BBÖBŢB��B��B��B��B�`B�B\B �B�B�B�B�BuB�B �B/B%�B�BuB�B�B"�B%�B%�B$�B"�B�B'�BA�B>wB9XB49B-B$�BuBB��B�ZB��B�XB��B�%B\)B9XB-BoB
�BB
��B
q�B
iyB
bNB
L�B
)�B	�B	��B	S�B	-B	#�B	�B	�B	�B	�B	B��B�ZB�/B�#B�B�B��B��B��BǮB��B��BBƨB��BǮB��B��B��B�B�
B��B��BȴBǮBƨBƨBĜBB�wB�dB�^B��B��B��BɺB��B��BɺBƨB��BÖBɺB��B��B�)B�B�B�B�B�B�B��B��B��B	  B	B	VB	oB	PB	JB	PB	\B	\B	{B	�B	�B	�B	$�B	/B	2-B	5?B	:^B	B�B	K�B	A�B	8RB	:^B	:^B	8RB	49B	1'B	'�B	%�B	&�B	'�B	)�B	2-B	49B	1'B	0!B	0!B	6FB	<jB	=qB	=qB	A�B	F�B	I�B	I�B	J�B	L�B	VB	YB	[#B	\)B	]/B	\)B	[#B	[#B	]/B	cTB	ffB	gmB	q�B	r�B	q�B	q�B	r�B	t�B	r�B	v�B	|�B	|�B	� B	�B	�B	�B	�B	�B	}�B	z�B	y�B	x�B	|�B	y�B	u�B	t�B	|�B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�FB	�LB	�dB	�dB	�^B	�dB	�dB	�dB	�dB	�jB	�wB	�}B	�}B	��B	��B	B	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�;B	�BB	�BB	�BB	�BB	�NB	�NB	�TB	�ZB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
DB

=B
	7B
%B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
JB
PB
PB
PB
PB
\B
�B
S222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB��B�B�5B�yB	=qB
N�B
�B
�!B
��B
�B
��BBJBJBbB�B�B�B�B�B �B'�B,B(�B)�B+B/B6FB;dB:^BC�BF�BA�B>wBA�BF�BL�B[#Bq�B�B�{B��B�B�'B�9B�}BBÖBŢB��B��B��B��B�`B�B\B �B�B�B�B�BuB�B �B/B%�B�BuB�B�B"�B%�B%�B$�B"�B�B'�BA�B>wB9XB49B-B$�BuBB��B�ZB��B�XB��B�%B\)B9XB-BoB
�BB
��B
q�B
iyB
bNB
L�B
)�B	�B	��B	S�B	-B	#�B	�B	�B	�B	�B	B��B�ZB�/B�#B�B�B��B��B��BǮB��B��BBƨB��BǮB��B��B��B�B�
B��B��BȴBǮBƨBƨBĜBB�wB�dB�^B��B��B��BɺB��B��BɺBƨB��BÖBɺB��B��B�)B�B�B�B�B�B�B��B��B��B	  B	B	VB	oB	PB	JB	PB	\B	\B	{B	�B	�B	�B	$�B	/B	2-B	5?B	:^B	B�B	K�B	A�B	8RB	:^B	:^B	8RB	49B	1'B	'�B	%�B	&�B	'�B	)�B	2-B	49B	1'B	0!B	0!B	6FB	<jB	=qB	=qB	A�B	F�B	I�B	I�B	J�B	L�B	VB	YB	[#B	\)B	]/B	\)B	[#B	[#B	]/B	cTB	ffB	gmB	q�B	r�B	q�B	q�B	r�B	t�B	r�B	v�B	|�B	|�B	� B	�B	�B	�B	�B	�B	}�B	z�B	y�B	x�B	|�B	y�B	u�B	t�B	|�B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�FB	�LB	�dB	�dB	�^B	�dB	�dB	�dB	�dB	�jB	�wB	�}B	�}B	��B	��B	B	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�;B	�BB	�BB	�BB	�BB	�NB	�NB	�TB	�ZB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
DB

=B
	7B
%B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
JB
PB
PB
PB
PB
\B
�B
S222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190608                              AO  ARCAADJP                                                                    20181005190608    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190608  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190608  QCF$                G�O�G�O�G�O�8000            