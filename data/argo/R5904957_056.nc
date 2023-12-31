CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:15Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140815  20181024140815  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               8A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׽�&�J1   @׽���1"@3!$�/�c��1&�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      8A   A   B   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B��B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ�C\  C^  C`  Ca�fCd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD �fD  D� D  D�fD  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD�fD  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D�fD  D� D��D� D  D� DfD� D  D� D  D�fDfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(y�D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D<��D=y�D=��D>� D?  D?� D@  D@�fDAfDA�fDBfDB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DMfDM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DVy�DW  DW� DX  DXy�DY  DY�fDZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Da��Db� Dc  Dc� DdfDd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� DmfDm� Dm��Dny�Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dw�fDy�\D�1�D�|)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@��
A�A#�AC�Ac�A�A�A�A�A�A�A�A�B �HB�HB�HBz�B z�B(�HB0�HB8�HB@�HBH�HBP�HBYG�B`�HBh�HBp�HBx�HB�p�B�p�B�p�B�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB8RCDQ�CF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV8RCXQ�CZQ�C\8RC^8RC`8RCb�Cd8RCf8RCh8RCjQ�Cl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|�C~8RC�)C�)C�)C�(�C�)C�)C�(�C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�\C�\C�\C�)C�)C�\C�)C�)C�)C�(�C�)C�)C�\C�)C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�\C�)C�)C�)C�)C�)C�)C�)C�)C�)D zD �zDD�DD�zDD�DD�DD�zDD�DD�DD�D	D	�D
D
�DD�DD�DzD�zDD�DD�DD�DzD�DD�DD�DD�DD�zDD�D�D�DD�DzD�DD�DD�zDzD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(�D(��D)D)�zD*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0zD0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�zD<D<�D=�D=��D>�D>�D?D?�D@D@�zDAzDA�zDBzDB�DCDC�DDDD�DEDE�DF�DF�DGDG�DHDH�DIDI�DJzDJ�DKDK�DLDL�DMzDM�zDNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DV�DV��DWDW�DXDX��DYDY�zDZDZ��D[D[�D\D\�D]D]�D^D^�D_D_�D`�D`�DaDa�Db�Db�DcDc�DdzDd�zDeDe�DfDf�DgDg�DhDh�DiDi�DjzDj�DkDk�DlDl�DmzDm�Dn�Dn��DoDo�DpDp��DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�zDwDw�Dw�zDy�pD�8�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A��A��A��A�(�A�"�A�(�A�-A�+A�-A�+A�-A�-A�(�A�$�A�oA��A��A�(�A��A���AՋDA�dZA��A�A��Aԟ�A�S�A�bAӺ^A�;dAҏ\A��A�E�AиRAЩ�A�1A�ȴA���A�`BA�G�A�\)A�S�Aȡ�A�oAƥ�A�M�A��A�E�A�{A���AÓuA��\A��A��`A�+A�K�A�A�A��yA�t�A���A�dZA��A�ƨA��!A��PA�A�A��`A���A��^A�=qA��jA�x�A�A��A�jA���A�{A��HA��A��A�5?A��A��A�Q�A�hsA��A���A� �A��
A�z�A�(�A��A��wA�C�A�5?A�\)AO�A|=qAzQ�AwAtJAq\)Ao��An�AlĜAjz�Ag�-Ab�A_��A]�TA[&�AZn�AX �AS�AP1'ANȴALbNAI�AG��AE�ADJABE�AAVA?�A?A=�
A<I�A;�A;�-A;S�A:v�A8��A6�`A6-A4��A25?A1�;A0�A.�`A-�;A,�/A+��A+oA(�RA&jA%�mA$�9A#�A#;dA!�hA�^A��A�;A��A�hA/A�HA5?A5?Ar�A(�A��A��AG�A"�A��A��Ax�AbNA��AA�jA
=AQ�A`BA
�/A
M�AZA��A1'A�FA��AVA 9X@���@�{@�V@�Z@�C�@��@�P@�9X@�hs@���@�5?@�Z@�@�n�@�@�dZ@���@��@��@ܼj@���@��H@���@��@և+@ָR@պ^@���@�I�@Ұ!@�V@��
@ղ-@ٺ^@�9@�hs@���@�Z@���@�%@�C�@ݺ^@ݙ�@ݺ^@�v�@ޟ�@�E�@��T@�Ĝ@ڸR@��/@���@�l�@�@��@�bN@�I�@�9X@�@���@�Ĝ@�(�@ˮ@ˍP@�|�@�+@�x�@�%@���@�Ĝ@�(�@ǍP@�33@��@ư!@�E�@��#@��@�  @��H@�v�@��@��@���@�&�@���@��y@���@�J@���@�O�@�?}@�?}@�/@�%@��u@�C�@�n�@�=q@���@�&�@���@�A�@�dZ@��@��H@��\@��^@���@�(�@��@�E�@�M�@��^@�%@���@���@�bN@�(�@�33@��y@���@��!@�ff@��@�7L@��@�%@��D@�r�@�9X@��
@���@�dZ@�dZ@�"�@��R@���@�@��-@�p�@�&�@��/@�r�@���@�S�@��R@��!@���@��\@�ff@��@�@��7@�X@�G�@�7L@�%@��/@��@��u@��@�Q�@���@�ƨ@�dZ@�S�@�+@��@�ȴ@���@�~�@�5?@���@��@��/@��@�  @��F@�K�@���@��\@�v�@�^5@�V@�=q@���@���@��@�p�@�O�@�7L@��@���@���@�Ĝ@��9@�z�@�1'@�  @���@���@���@�  @�  @��@���@��w@���@�33@���@�ff@���@���@�X@��@��@���@�9X@�ƨ@��@�\)@�C�@��@��y@�n�@�$�@�J@�@���@��@�hs@�V@��j@��;@��y@��!@�^5@�@��^@�X@�%@��/@��j@�r�@� �@��m@���@�dZ@�"�@�
=@��H@�V@��@���@��#@���@��^@���@�/@��@�r�@�b@��@�+@��y@���@��@��@�@��7@�`B@��@��/@�z�@�I�@�1@���@��@��m@�dZ@���@��@��@��R@���@��\@�V@t,=@dg81111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A��A��A��A�(�A�"�A�(�A�-A�+A�-A�+A�-A�-A�(�A�$�A�oA��A��A�(�A��A���AՋDA�dZA��A�A��Aԟ�A�S�A�bAӺ^A�;dAҏ\A��A�E�AиRAЩ�A�1A�ȴA���A�`BA�G�A�\)A�S�Aȡ�A�oAƥ�A�M�A��A�E�A�{A���AÓuA��\A��A��`A�+A�K�A�A�A��yA�t�A���A�dZA��A�ƨA��!A��PA�A�A��`A���A��^A�=qA��jA�x�A�A��A�jA���A�{A��HA��A��A�5?A��A��A�Q�A�hsA��A���A� �A��
A�z�A�(�A��A��wA�C�A�5?A�\)AO�A|=qAzQ�AwAtJAq\)Ao��An�AlĜAjz�Ag�-Ab�A_��A]�TA[&�AZn�AX �AS�AP1'ANȴALbNAI�AG��AE�ADJABE�AAVA?�A?A=�
A<I�A;�A;�-A;S�A:v�A8��A6�`A6-A4��A25?A1�;A0�A.�`A-�;A,�/A+��A+oA(�RA&jA%�mA$�9A#�A#;dA!�hA�^A��A�;A��A�hA/A�HA5?A5?Ar�A(�A��A��AG�A"�A��A��Ax�AbNA��AA�jA
=AQ�A`BA
�/A
M�AZA��A1'A�FA��AVA 9X@���@�{@�V@�Z@�C�@��@�P@�9X@�hs@���@�5?@�Z@�@�n�@�@�dZ@���@��@��@ܼj@���@��H@���@��@և+@ָR@պ^@���@�I�@Ұ!@�V@��
@ղ-@ٺ^@�9@�hs@���@�Z@���@�%@�C�@ݺ^@ݙ�@ݺ^@�v�@ޟ�@�E�@��T@�Ĝ@ڸR@��/@���@�l�@�@��@�bN@�I�@�9X@�@���@�Ĝ@�(�@ˮ@ˍP@�|�@�+@�x�@�%@���@�Ĝ@�(�@ǍP@�33@��@ư!@�E�@��#@��@�  @��H@�v�@��@��@���@�&�@���@��y@���@�J@���@�O�@�?}@�?}@�/@�%@��u@�C�@�n�@�=q@���@�&�@���@�A�@�dZ@��@��H@��\@��^@���@�(�@��@�E�@�M�@��^@�%@���@���@�bN@�(�@�33@��y@���@��!@�ff@��@�7L@��@�%@��D@�r�@�9X@��
@���@�dZ@�dZ@�"�@��R@���@�@��-@�p�@�&�@��/@�r�@���@�S�@��R@��!@���@��\@�ff@��@�@��7@�X@�G�@�7L@�%@��/@��@��u@��@�Q�@���@�ƨ@�dZ@�S�@�+@��@�ȴ@���@�~�@�5?@���@��@��/@��@�  @��F@�K�@���@��\@�v�@�^5@�V@�=q@���@���@��@�p�@�O�@�7L@��@���@���@�Ĝ@��9@�z�@�1'@�  @���@���@���@�  @�  @��@���@��w@���@�33@���@�ff@���@���@�X@��@��@���@�9X@�ƨ@��@�\)@�C�@��@��y@�n�@�$�@�J@�@���@��@�hs@�V@��j@��;@��y@��!@�^5@�@��^@�X@�%@��/@��j@�r�@� �@��m@���@�dZ@�"�@�
=@��H@�V@��@���@��#@���@��^@���@�/@��@�r�@�b@��@�+@��y@���@��@��@�@��7@�`B@��@��/@�z�@�I�@�1@���@��@��m@�dZ@���@��@��@��R@���@��\@�V@t,=@dg81111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
E�B
B�B
@�B
=qB
C�B
D�B
F�B
G�B
F�B
D�B
B�B
@�B
?}B
=qB
?}B
:^B
YB
�B
�oB
��B
�B
�qB
ɺB
��B
ɺB
ĜB
�
B)�Bw�B��B��B�FB��B�#B�ZB�?��B�DB�DB�VB�PB�Bp�BbNB[#B_;B\)BZBYBS�BN�BF�B5?B&�B�B��B��B�B�ZB��B�oB�B�B|�Bs�BiyB[#BP�BL�B=qB"�B�BDB
�B
�B
�3B
��B
�+B
|�B
n�B
\)B
>wB
$�B
�B
%B	��B	�B	�BB	�)B	�B	��B	�?B	��B	�%B	z�B	l�B	e`B	Q�B	8RB	/B	%�B	�B	B��B�B�B�`B�)B�B�
B��BɺBȴBȴBƨBŢBÖB�}B�dB�LB�3B�-B�!B�B��B��B��B��B�DB�7B�7B�\B�{B��B�uB�=B�B�oB��B��B��B��B��B�B�RB�LB�B��B��B�B�wB��B��B��B�wB��B�VB�By�Bw�B{�B|�Br�Be`Be`BdZBffBcTBdZBaHBaHBaHB`BB^5B`BB]/B\)B\)B[#BYBXBW
BT�BT�BXBT�BQ�BO�BK�BJ�BI�BH�BI�BK�BQ�BT�BZB_;BbNBhsBv�B�+B��B��B�BB�`B�HB�sB�B�yB�mB�sB�yB�B��B�B��B�B�B�mB�/B�B��B��B��B��B��B��B�B�B�#B�)B�)B�)B�)B�HB�NB�NB�TB�`B�mB�sB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B	%B		7B	DB	VB	oB	{B	{B	�B	�B	!�B	%�B	&�B	)�B	.B	0!B	2-B	7LB	9XB	9XB	:^B	>wB	E�B	L�B	P�B	W
B	W
B	\)B	`BB	aHB	aHB	e`B	hsB	r�B	v�B	w�B	w�B	y�B	}�B	~�B	� B	�B	�B	�B	�+B	�=B	�DB	�JB	�JB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�'B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�LB	�LB	�XB	�XB	�XB	�^B	�dB	�dB	�jB	�qB	��B	B	ÖB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�/B	�/B	�5B	�;B	�HB	�NB	�NB	�NB	�TB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
JB
�B
$�B
61111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
E�B
B�B
@�B
=qB
C�B
D�B
F�B
G�B
F�B
D�B
B�B
@�B
?}B
=qB
?}B
:^B
YB
�B
�oB
��B
�B
�qB
ɺB
��B
ɺB
ĜB
�
B)�Bw�B��B��B�FB��B�#B�ZB�?��B�DB�DB�VB�PB�Bp�BbNB[#B_;B\)BZBYBS�BN�BF�B5?B&�B�B��B��B�B�ZB��B�oB�B�B|�Bs�BiyB[#BP�BL�B=qB"�B�BDB
�B
�B
�3B
��B
�+B
|�B
n�B
\)B
>wB
$�B
�B
%B	��B	�B	�BB	�)B	�B	��B	�?B	��B	�%B	z�B	l�B	e`B	Q�B	8RB	/B	%�B	�B	B��B�B�B�`B�)B�B�
B��BɺBȴBȴBƨBŢBÖB�}B�dB�LB�3B�-B�!B�B��B��B��B��B�DB�7B�7B�\B�{B��B�uB�=B�B�oB��B��B��B��B��B�B�RB�LB�B��B��B�B�wB��B��B��B�wB��B�VB�By�Bw�B{�B|�Br�Be`Be`BdZBffBcTBdZBaHBaHBaHB`BB^5B`BB]/B\)B\)B[#BYBXBW
BT�BT�BXBT�BQ�BO�BK�BJ�BI�BH�BI�BK�BQ�BT�BZB_;BbNBhsBv�B�+B��B��B�BB�`B�HB�sB�B�yB�mB�sB�yB�B��B�B��B�B�B�mB�/B�B��B��B��B��B��B��B�B�B�#B�)B�)B�)B�)B�HB�NB�NB�TB�`B�mB�sB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B	%B		7B	DB	VB	oB	{B	{B	�B	�B	!�B	%�B	&�B	)�B	.B	0!B	2-B	7LB	9XB	9XB	:^B	>wB	E�B	L�B	P�B	W
B	W
B	\)B	`BB	aHB	aHB	e`B	hsB	r�B	v�B	w�B	w�B	y�B	}�B	~�B	� B	�B	�B	�B	�+B	�=B	�DB	�JB	�JB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�'B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�LB	�LB	�XB	�XB	�XB	�^B	�dB	�dB	�jB	�qB	��B	B	ÖB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�/B	�/B	�5B	�;B	�HB	�NB	�NB	�NB	�TB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
JB
�B
$�B
61111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140815                              AO  ARCAADJP                                                                    20181024140815    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140815  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140815  QCF$                G�O�G�O�G�O�0               