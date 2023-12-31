CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:57Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191657  20181005191657  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               'A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׹$�Uv1   @׹%��̎@5-�����c�$�/�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      'A   A   A   @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ�C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp�Cr  Cs�fCv  Cx  Cz  C|  C}�fC�  C�  C�  C��3C�  C��C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C��3C�  C��3C��3C��C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C�  C��C��3C�  C�  C��3C��3C��3C��3C��fC��3C�  C��3C��3C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C��3C�  C��C��C�  C��fC��C��C�  C�  C��C�  C��3D � D ��D� DfDy�D��D� DfD� D  D� D  D� D  D� DfDy�D	  D	� D
  D
y�D  Dy�D  D� D  D�fDfD� D��Dy�D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D$��D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D+��D,�fD-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D4��D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DP��DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DWfDWy�DXfDXy�DY  DY� DZ  DZ� D[  D[y�D\  D\y�D]  D]� D^fD^� D_  D_y�D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dq��Dr�fDr��Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dwy�Dw�fDy~D�;�D�}q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @AG�@��
@�
=A�A#�AC�Ac�A�A�A�A�A\A�A�A�B �HB�HB�HB�HB �HB(�HB1G�B9G�B@�HBH�HBP�HBX�HB`z�Bh�HBp�HBx�HB�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B���B�p�B�p�B�p�B�=qB�p�B�p�B�p�B�p�B�p�B�=qB�p�B�p�B�p�B�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C 8RC8RC�C8RC8RC
8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC 8RC"Q�C$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB8RCD8RCF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV�CX8RCZQ�C\8RC^8RC`8RCb�Cd�Cf8RCh8RCj8RCl8RCn8RCpQ�Cr8RCt�Cv8RCx8RCz8RC|8RC~�C�)C�)C�)C�\C�)C�(�C�)C�\C�)C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�)C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�)C�)C�)C�(�C�)C�)C�)C�(�C�)C�)C�)C�)C�(�C�)C�)C�)C�\C�)C�)C�)C�)C�)C�\C�(�C�)C�\C�\C�)C�)C�)C�)C�)C�)C�)C�)C�\C�\C�)C�)C�\C�\C�\C�\C�)C�\C�\C�(�C�)C�)C�)C�(�C�)C�)C�\C�)C�)C�\C�)C�(�C�\C�)C�)C�\C�\C�\C�\C��C�\C�)C�\C�\C�)C�(�C�)C�)C�(�C�(�C�)C�)C�)C�)C�)C�(�C�)C�)C�(�C�(�C�(�C�\C�)C�(�C�(�C�)C��C�(�C�(�C�)C�)C�(�C�)D �D �D�D�DzD��D�D�DzD�DD�DD�DD�DzD��D	D	�D
D
��DD��DD�DD�zDzD�D�D��DD�DD�D�D��DD�DD�DD�DD�DD�DD�DD�D�D�zDD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$zD$�D%�D%�D&�D&�D'D'�D(D(�D)D)�D*D*�zD+D+�D,�D,�zD-D-�D.D.�D/zD/�D0D0�D1D1�D2�D2�D3D3�D4D4�D5�D5�D6zD6�D7D7�D8D8�D9D9�D:D:��D;D;�D<D<�D=D=�D>D>�D?D?�D@�D@�DADA�DBDB��DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMzDM�DNDN�DODO�DPDP�DQ�DQ�zDRDR�DSDS�DTDT�DUDU�DVDV��DWzDW��DXzDX��DYDY�DZDZ�D[D[��D\D\��D]D]�D^zD^�D_D_��D`D`�DaDa��DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi��DjDj�zDkDk�DlDl�DmDm��DnDn�DozDo�DpDp�DqDq�Dr�Dr�zDs�Ds�DtDt�DuDu�zDvDv�DwDw��Dw�zDy�(D�B�D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aו�Aו�Aו�AדuAו�Aו�Aח�Aח�Aח�Aח�AדuAבhA׉7A׋DA׃A�v�A��A�"�Aԡ�A��mAӃA�E�A��
A�jA�7LAа!A�AͲ-A˛�A�JAɡ�A�x�AȑhA�n�AƝ�AŴ9A�XA�{A�ĜA��-A�t�A���A��
A��`A��RA�bA��A���A���A�  A��A�9XA��/A��HA�ffA�t�A��A��!A�C�A���A�&�A�I�A�VA���A�E�A���A�1A�A�A�jA�+A��mA���A��
A��RA�M�A��
A�ZA��A���A���A���A��A�~�A���A�;dA��7A�7LA���A��A�bNA��`A���A��A��A�JA�ĜA���A�=qA~n�A{��AzbNAzbNAyO�AvȴAt��As7LAr�`ArQ�Aq��Ap��An��Ai\)Af�Ael�Ac�Abv�A^bA[?}AY��AY|�AX1'AU��AT�AS��AQ��AP��AO��AM�#AJ�AI�PAH�AG�TAGt�AG/AF��AES�AD��ADQ�AA��A?�A>~�A=�hA=�A<JA9\)A8  A7�A6��A5ƨA4�!A3G�A2bA/�^A-/A*�A(�A't�A&I�A%|�A%XA%?}A%?}A%C�A%;dA%7LA%33A%&�A#A"bA �!A|�A�A��A%AXAp�A��A�TA&�A�!AJAr�A��A��Az�A(�AZAl�A
=A
�A	C�A5?AoA�uAƨA��AA�9A?}@��!@�@�X@���@��+@�x�@��@���@��^@���@� �@�;d@���@�o@�E�@�E�@�=q@��@�p�@��@��@�z�@� �@�V@��@��@�h@��/@���@�o@ޟ�@�V@��T@�5?@�=q@ݩ�@�5?@��@ו�@�-@ղ-@Ցh@Ձ@�Z@�(�@� �@ӝ�@�t�@�|�@�+@��#@��@�l�@ͩ�@�p�@�O�@�&�@̼j@�Z@��@�1@˕�@�"�@�5?@�X@Ȭ@�b@Ǯ@�n�@ŉ7@ģ�@Å@���@��@+@��T@�7L@��`@�r�@��m@�t�@���@��R@���@�/@���@�j@��F@�+@���@��+@�=q@�J@��#@�hs@���@��
@�t�@�t�@�\)@�@���@��\@�J@��^@�`B@���@�r�@�b@��@��P@��@�t�@�33@���@���@�{@���@���@�@��^@���@��h@�`B@�?}@��@���@���@���@���@�j@��;@�\)@���@�$�@���@��@��@��@���@���@�7L@��w@�\)@�@���@�n�@�V@�$�@��-@��h@��h@�@�p�@�&�@��@�9X@��m@���@��P@�dZ@�S�@�"�@��@��@���@��P@�;d@���@��y@��@���@�ȴ@���@��R@��R@��!@��!@��R@��R@���@���@���@���@���@���@�%@��m@�dZ@�+@���@��\@�@���@�`B@�&�@�V@��`@���@�Q�@��@��
@���@�l�@�dZ@�C�@�"�@��H@�ȴ@���@�~�@�$�@���@�hs@�X@�G�@�&�@���@��@��/@���@��9@�I�@�b@��w@�t�@��H@��R@�ff@�$�@��^@�x�@�G�@��@�b@���@�+@��\@�E�@�=q@�$�@�{@�{@���@���@���@���@�7L@�(�@�|�@�@���@��H@���@��\@�~�@�v�@�^5@�=q@��@�{@�@���@��@�O�@�V@���@��j@��9@��@��@�bN@�bN@�bN@�Z@��@�l�@�M�@���@�@�%@��/@��/@��/@��/@��/@���@��9@��@���@���@���@���@��u@��D@��@�1'@�hs@�\)@qj1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aו�Aו�Aו�AדuAו�Aו�Aח�Aח�Aח�Aח�AדuAבhA׉7A׋DA׃A�v�A��A�"�Aԡ�A��mAӃA�E�A��
A�jA�7LAа!A�AͲ-A˛�A�JAɡ�A�x�AȑhA�n�AƝ�AŴ9A�XA�{A�ĜA��-A�t�A���A��
A��`A��RA�bA��A���A���A�  A��A�9XA��/A��HA�ffA�t�A��A��!A�C�A���A�&�A�I�A�VA���A�E�A���A�1A�A�A�jA�+A��mA���A��
A��RA�M�A��
A�ZA��A���A���A���A��A�~�A���A�;dA��7A�7LA���A��A�bNA��`A���A��A��A�JA�ĜA���A�=qA~n�A{��AzbNAzbNAyO�AvȴAt��As7LAr�`ArQ�Aq��Ap��An��Ai\)Af�Ael�Ac�Abv�A^bA[?}AY��AY|�AX1'AU��AT�AS��AQ��AP��AO��AM�#AJ�AI�PAH�AG�TAGt�AG/AF��AES�AD��ADQ�AA��A?�A>~�A=�hA=�A<JA9\)A8  A7�A6��A5ƨA4�!A3G�A2bA/�^A-/A*�A(�A't�A&I�A%|�A%XA%?}A%?}A%C�A%;dA%7LA%33A%&�A#A"bA �!A|�A�A��A%AXAp�A��A�TA&�A�!AJAr�A��A��Az�A(�AZAl�A
=A
�A	C�A5?AoA�uAƨA��AA�9A?}@��!@�@�X@���@��+@�x�@��@���@��^@���@� �@�;d@���@�o@�E�@�E�@�=q@��@�p�@��@��@�z�@� �@�V@��@��@�h@��/@���@�o@ޟ�@�V@��T@�5?@�=q@ݩ�@�5?@��@ו�@�-@ղ-@Ցh@Ձ@�Z@�(�@� �@ӝ�@�t�@�|�@�+@��#@��@�l�@ͩ�@�p�@�O�@�&�@̼j@�Z@��@�1@˕�@�"�@�5?@�X@Ȭ@�b@Ǯ@�n�@ŉ7@ģ�@Å@���@��@+@��T@�7L@��`@�r�@��m@�t�@���@��R@���@�/@���@�j@��F@�+@���@��+@�=q@�J@��#@�hs@���@��
@�t�@�t�@�\)@�@���@��\@�J@��^@�`B@���@�r�@�b@��@��P@��@�t�@�33@���@���@�{@���@���@�@��^@���@��h@�`B@�?}@��@���@���@���@���@�j@��;@�\)@���@�$�@���@��@��@��@���@���@�7L@��w@�\)@�@���@�n�@�V@�$�@��-@��h@��h@�@�p�@�&�@��@�9X@��m@���@��P@�dZ@�S�@�"�@��@��@���@��P@�;d@���@��y@��@���@�ȴ@���@��R@��R@��!@��!@��R@��R@���@���@���@���@���@���@�%@��m@�dZ@�+@���@��\@�@���@�`B@�&�@�V@��`@���@�Q�@��@��
@���@�l�@�dZ@�C�@�"�@��H@�ȴ@���@�~�@�$�@���@�hs@�X@�G�@�&�@���@��@��/@���@��9@�I�@�b@��w@�t�@��H@��R@�ff@�$�@��^@�x�@�G�@��@�b@���@�+@��\@�E�@�=q@�$�@�{@�{@���@���@���@���@�7L@�(�@�|�@�@���@��H@���@��\@�~�@�v�@�^5@�=q@��@�{@�@���@��@�O�@�V@���@��j@��9@��@��@�bN@�bN@�bN@�Z@��@�l�@�M�@���@�@�%@��/@��/@��/@��/@��/@���@��9@��@���@���@���@���@��u@��D@��@�1'@�hs@�\)@qj1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BoBoBoBoBoBoBoBoBoBoBoBuBuBuBuBuB�B9XBiyBz�B�B�B�7B�\B��B��B��B��B�^BĜB��B�B��B\B7LBD�BP�BYBS�BO�BW
B]/Bk�Bp�B}�B��B�'B��B��B�'B�'B�B�B��B��BB�B��B��B�B�5BĜB�jB�}BȴBÖBŢB��B�-B��B�JBv�BbNBQ�BF�B<jB1'BB�/B�dB��B�{B�bBs�BVBE�B+B
��B
�B
�`B
�#B
��B
�3B
�B
dZB
O�B
5?B
(�B
{B
�B
�B
(�B
-B
�B
hB
%B
B	��B	��B	�B	��B	��B	�DB	�B	t�B	iyB	S�B	F�B	>wB	:^B	49B	)�B	$�B	!�B	�B	�B	�B	PB	B��B��B��B�B�B�B�B�mB�ZB�#B��B��B��BɺBĜB�wB�^B�XB�FB�3B�!B�B��B��B��B��B�hB�VB�VB�hB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�hB�hB�hB�bB�VB�JB�=B�=B�PB�VB�bB�bB�oB�{B�uB��B�\B�=B�=B�PB�\B�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�dB�jB�jB�wB�wB�}BBȴB��B��B��B��B�B�B�
B�B�#B�B�B�B�/B�)B�;B�fB�mB�B�B�B��B��B��B	  B	B	
=B	PB	\B	VB	VB	PB	VB	oB	�B	�B	�B	�B	$�B	%�B	(�B	,B	/B	0!B	0!B	49B	8RB	8RB	8RB	9XB	<jB	?}B	?}B	B�B	D�B	F�B	H�B	I�B	K�B	M�B	N�B	N�B	O�B	P�B	S�B	T�B	W
B	XB	XB	YB	YB	ZB	\)B	]/B	_;B	e`B	m�B	p�B	r�B	u�B	x�B	|�B	}�B	}�B	|�B	|�B	|�B	|�B	|�B	}�B	}�B	{�B	{�B	}�B	� B	�B	�B	�B	�%B	�DB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�-B	�9B	�?B	�?B	�?B	�LB	�RB	�LB	�3B	�3B	�9B	�^B	�^B	�dB	�jB	�wB	�}B	��B	ÖB	ĜB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�#B	�)B	�/B	�5B	�BB	�HB	�TB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
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
VB
�B
[B
 �2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BoBoBoBoBoBoBoBoBoBoBoBuBuBuBuBuB�B9XBiyBz�B�B�B�7B�\B��B��B��B��B�^BĜB��B�B��B\B7LBD�BP�BYBS�BO�BW
B]/Bk�Bp�B}�B��B�'B��B��B�'B�'B�B�B��B��BB�B��B��B�B�5BĜB�jB�}BȴBÖBŢB��B�-B��B�JBv�BbNBQ�BF�B<jB1'BB�/B�dB��B�{B�bBs�BVBE�B+B
��B
�B
�`B
�#B
��B
�3B
�B
dZB
O�B
5?B
(�B
{B
�B
�B
(�B
-B
�B
hB
%B
B	��B	��B	�B	��B	��B	�DB	�B	t�B	iyB	S�B	F�B	>wB	:^B	49B	)�B	$�B	!�B	�B	�B	�B	PB	B��B��B��B�B�B�B�B�mB�ZB�#B��B��B��BɺBĜB�wB�^B�XB�FB�3B�!B�B��B��B��B��B�hB�VB�VB�hB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�hB�hB�hB�bB�VB�JB�=B�=B�PB�VB�bB�bB�oB�{B�uB��B�\B�=B�=B�PB�\B�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�dB�jB�jB�wB�wB�}BBȴB��B��B��B��B�B�B�
B�B�#B�B�B�B�/B�)B�;B�fB�mB�B�B�B��B��B��B	  B	B	
=B	PB	\B	VB	VB	PB	VB	oB	�B	�B	�B	�B	$�B	%�B	(�B	,B	/B	0!B	0!B	49B	8RB	8RB	8RB	9XB	<jB	?}B	?}B	B�B	D�B	F�B	H�B	I�B	K�B	M�B	N�B	N�B	O�B	P�B	S�B	T�B	W
B	XB	XB	YB	YB	ZB	\)B	]/B	_;B	e`B	m�B	p�B	r�B	u�B	x�B	|�B	}�B	}�B	|�B	|�B	|�B	|�B	|�B	}�B	}�B	{�B	{�B	}�B	� B	�B	�B	�B	�%B	�DB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�-B	�9B	�?B	�?B	�?B	�LB	�RB	�LB	�3B	�3B	�9B	�^B	�^B	�dB	�jB	�wB	�}B	��B	ÖB	ĜB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�#B	�)B	�/B	�5B	�BB	�HB	�TB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
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
VB
�B
[B
 �2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191657                              AO  ARCAADJP                                                                    20181005191657    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191657  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191657  QCF$                G�O�G�O�G�O�8000            