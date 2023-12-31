CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:38Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191738  20181005191738  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����cJ1   @���-��@5������d�+I�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�33@�  A   AffA>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C[�fC]�fC_�fCb  Cd  Cf  Ch�Cj�Cl  Cn  Cp�Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C��C�  C��3C��3C��3C��C��C��C��C�  C�  C��C��C��C��C�  C��3C�  C�  C��3C��C��C��C��C��C�  C�  C�  C�  C��3C��fC��3C��3C��3C��3C�  C�  C�  C��C�  C�  C��C��C��C��C��C�  C��C�  C�  C�  C�  C�  C��C��3C��C�  C�  C��3C��3C��C��3C��3C�  C��3C��C�  C�  C��C��C��C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��C��C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C��C�  C��3C�  C��3D   D � D ��D� DfD�fDfD� D��D� D��D� DfD� D  D� DfD�fD	  D	y�D	��D
y�DfD�fDfD�fD  D� D��Dy�D  D� D�3D� DfD� D��D� D  D� D  D�fD  D� DfD� D  D� D  Dy�D  D� DfD�fDfD�fDfD� D��D� D  Dy�D��D� D   D y�D ��D!�fD"fD"� D#  D#y�D#��D$�fD%  D%y�D&  D&�fD'fD'��D(  D(y�D)  D)� D*  D*� D+fD+� D+��D,y�D,��D-y�D.fD.�fD/  D/y�D0fD0� D1  D1� D2  D2� D3  D3y�D4  D4� D5fD5� D5��D6� D7  D7� D8  D8� D9fD9�fD:fD:�fD;fD;� D<  D<�fD=  D=y�D=��D>y�D>��D?y�D?��D@� D@��DAy�DB  DB�fDCfDC�fDDfDD� DE  DEy�DF  DF�fDF��DG� DHfDH� DIfDI� DI��DJ� DJ��DK� DL  DL� DMfDM� DN  DN� DN��DO� DPfDP�fDQfDQ�fDRfDR�fDSfDS� DS��DT�fDU  DUy�DV  DV� DW  DW�fDW��DXy�DY  DY�fDZ  DZ� D[  D[�fD\  DhfDh��DifDi� Dj�Dj�fDkfDky�Dl  Dl� DmfDm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� DufDu� Dv  Dv� Dw  Dw� Dw��Dy|)D�4�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@\AG�A�A?�A_�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(�RB0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�Bp�RBxQ�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C.C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX.CZ{C[��C]��C_��Cb{Cd{Cf{Ch.Cj.Cl{Cn{Cp.Cr.Ct.Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C��pC�
=C�
C�
C�
C�
C�
C�
=C��pC��pC��pC�
C�
C�
C�
C�
=C�
=C�
C�
C�
C�#�C�
=C��pC�
=C�
=C��pC�
C�
C�#�C�
C�
C�
=C�
=C�
=C�
=C��pC��C��pC��pC��pC��pC�
=C�
=C�
=C�
C�
=C�
=C�
C�
C�
C�
C�
C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
C��pC�
C�
=C�
=C��pC��pC�
C��pC��pC�
=C��pC�
C�
=C�
=C�
C�
C�
C��pC�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C�
C�
=C�
C�
C�
=C�
=C�
C�
C�
=C�
=C�
=C�
=C�
C�
=C�
=C��pC�
=C�
=C��pC�
=C�
C�
=C�
=C�
=C�
=C��pC�
=C�
C�
=C�
=C��pC�
=C�
C�
=C��pC�
=C��pD D �D ��D�D�D��D�D�D��D�D��D�D�D�DD�D�D��D	D	~�D	��D
~�D�D��D�D��DD�D��D~�DD�D�RD�D�D�D��D�DD�DD��DD�D�D�DD�DD~�DD�D�D��D�D��D�D�D��D�DD~�D��D�D D ~�D ��D!��D"�D"�D#D#~�D#��D$��D%D%~�D&D&��D'�D'��D(D(~�D)D)�D*D*�D+�D+�D+��D,~�D,��D-~�D.�D.��D/D/~�D0�D0�D1D1�D2D2�D3D3~�D4D4�D5�D5�D5��D6�D7D7�D8D8�D9�D9��D:�D:��D;�D;�D<D<��D=D=~�D=��D>~�D>��D?~�D?��D@�D@��DA~�DBDB��DC�DC��DD�DD�DEDE~�DFDF��DF��DG�DH�DH�DI�DI�DI��DJ�DJ��DK�DLDL�DM�DM�DNDN�DN��DO�DP�DP��DQ�DQ��DR�DR��DS�DS�DS��DT��DUDU~�DVDV�DWDW��DW��DX~�DYDY��DZDZ�D[D[��D\Dh�Dh��Di�Di�Dj�Dj��Dk�Dk~�DlDl�Dm�Dm��DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�Dt�Dt�Du�Du�DvDv�DwDw�Dw��Dy�HD�7\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�%A�%A�A�1A�bA�bA�bA�VA�oA�bA�{A�{A�oA�oA��A��A��A��A��A��A��A��A��A��A�oA�{A��A���A���AʸRA�l�A�r�AȑhA��Aǣ�AǏ\A�VA��A�?}A�C�A�?}A�?}A�VAƣ�A�`BA�C�A�A�A��A��Aŝ�A�ĜA�M�A�A��wA��wA�+A��A��7A���A�r�A��A���A���A��PA�A��uA�r�A��jA���A�G�A��HA�A��#A�-A�x�A�r�A��FA�E�A�?}A�M�A�-A��A�%A�ȴA��/A���A�VA���A��9A�$�A���A�"�A��A��FA�9XA��uA�XA��/A�=qA�A�A���A�
=A��hA���A�-A��uA���A��7A��A�7LA�AA~��A~�+A|�9A|1A{K�Ay�-Aw�Au`BAtQ�ArĜAn�/Ak�mAj9XAi
=AhM�Ag�TAgK�Af��Ae��Ad�Aa��A`��A^�9A[��A["�AZ�!AXE�AVJAU��AU�
AU`BATQ�AS�FAR�AR(�AQ�AOƨAO"�AN-AL(�AI��AHz�AG?}AFv�AD�ACS�AB�AA33A@(�A>��A=��A:��A9��A8�RA7l�A65?A5\)A3��A21'A1+A0�A0  A,��A+
=A)�#A(�yA'�A'\)A&  A$v�A#��A#��A#C�A#�A"�A!t�A ��A �DA�A�A9XA|�Av�A�9A/AjA��A��A�A�9A��A�!A�A�A��A�-A?}A9XAx�A
1A	�hA��AA�A�An�AM�A�AG�A{AAbNA��A��AXA Ĝ@��@��@�V@�l�@��@�G�@��@��@�r�@�V@�J@��@�O�@�b@@�t�@�S�@�~�@���@���@���@���@��#@�%@�F@�V@�\@㝲@�  @�+@�@��
@�@�Ĝ@�l�@թ�@Ցh@��@�S�@�Ĝ@Ϯ@υ@υ@��y@��@ʗ�@���@ǍP@�-@�G�@Õ�@\@��h@�G�@�V@��@��#@�hs@� �@�C�@�;d@�"�@�
=@��!@�E�@�-@�J@��@�x�@��9@� �@�ƨ@���@�C�@�v�@�ff@�O�@�  @�S�@��@��@�~�@�=q@�O�@�Ĝ@��D@��@�b@���@�dZ@�"�@��H@���@�M�@���@��`@���@�1'@��@�S�@�;d@���@�V@���@�X@�V@�z�@��@��m@��@��P@�|�@�dZ@�;d@�M�@�5?@�-@�@��@���@�j@�9X@��@�K�@�o@�ȴ@��+@��@���@��7@��^@�@��-@��-@�hs@��j@��@�b@�  @��
@�\)@�"�@���@�n�@�-@���@��-@��7@�X@�?}@�7L@��@��@���@���@��@�|�@�;d@��y@���@���@��+@��@���@��-@��h@�p�@��`@���@�Ĝ@���@�Z@��
@���@��@���@�t�@�+@���@���@�^5@�{@���@��-@�`B@��@��@��@���@��@���@���@�z�@�9X@� �@�  @��@�S�@�33@�
=@��y@��!@�M�@�J@���@�O�@���@�1@��
@��@���@��\@��9@��u@��u@��@�z�@�Q�@�b@��@���@�|�@�\)@�;d@�
=@�ȴ@��+@��T@���@�z�@��D@���@�K�@��@�
=@���@��@���@��\@��+@�~�@�^5@�V@�-@���@��a@w33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%A�%A�A�1A�bA�bA�bA�VA�oA�bA�{A�{A�oA�oA��A��A��A��A��A��A��A��A��A��A�oA�{A��A���A���AʸRA�l�A�r�AȑhA��Aǣ�AǏ\A�VA��A�?}A�C�A�?}A�?}A�VAƣ�A�`BA�C�A�A�A��A��Aŝ�A�ĜA�M�A�A��wA��wA�+A��A��7A���A�r�A��A���A���A��PA�A��uA�r�A��jA���A�G�A��HA�A��#A�-A�x�A�r�A��FA�E�A�?}A�M�A�-A��A�%A�ȴA��/A���A�VA���A��9A�$�A���A�"�A��A��FA�9XA��uA�XA��/A�=qA�A�A���A�
=A��hA���A�-A��uA���A��7A��A�7LA�AA~��A~�+A|�9A|1A{K�Ay�-Aw�Au`BAtQ�ArĜAn�/Ak�mAj9XAi
=AhM�Ag�TAgK�Af��Ae��Ad�Aa��A`��A^�9A[��A["�AZ�!AXE�AVJAU��AU�
AU`BATQ�AS�FAR�AR(�AQ�AOƨAO"�AN-AL(�AI��AHz�AG?}AFv�AD�ACS�AB�AA33A@(�A>��A=��A:��A9��A8�RA7l�A65?A5\)A3��A21'A1+A0�A0  A,��A+
=A)�#A(�yA'�A'\)A&  A$v�A#��A#��A#C�A#�A"�A!t�A ��A �DA�A�A9XA|�Av�A�9A/AjA��A��A�A�9A��A�!A�A�A��A�-A?}A9XAx�A
1A	�hA��AA�A�An�AM�A�AG�A{AAbNA��A��AXA Ĝ@��@��@�V@�l�@��@�G�@��@��@�r�@�V@�J@��@�O�@�b@@�t�@�S�@�~�@���@���@���@���@��#@�%@�F@�V@�\@㝲@�  @�+@�@��
@�@�Ĝ@�l�@թ�@Ցh@��@�S�@�Ĝ@Ϯ@υ@υ@��y@��@ʗ�@���@ǍP@�-@�G�@Õ�@\@��h@�G�@�V@��@��#@�hs@� �@�C�@�;d@�"�@�
=@��!@�E�@�-@�J@��@�x�@��9@� �@�ƨ@���@�C�@�v�@�ff@�O�@�  @�S�@��@��@�~�@�=q@�O�@�Ĝ@��D@��@�b@���@�dZ@�"�@��H@���@�M�@���@��`@���@�1'@��@�S�@�;d@���@�V@���@�X@�V@�z�@��@��m@��@��P@�|�@�dZ@�;d@�M�@�5?@�-@�@��@���@�j@�9X@��@�K�@�o@�ȴ@��+@��@���@��7@��^@�@��-@��-@�hs@��j@��@�b@�  @��
@�\)@�"�@���@�n�@�-@���@��-@��7@�X@�?}@�7L@��@��@���@���@��@�|�@�;d@��y@���@���@��+@��@���@��-@��h@�p�@��`@���@�Ĝ@���@�Z@��
@���@��@���@�t�@�+@���@���@�^5@�{@���@��-@�`B@��@��@��@���@��@���@���@�z�@�9X@� �@�  @��@�S�@�33@�
=@��y@��!@�M�@�J@���@�O�@���@�1@��
@��@���@��\@��9@��u@��u@��@�z�@�Q�@�b@��@���@�|�@�\)@�;d@�
=@�ȴ@��+@��T@���@�z�@��D@���@�K�@��@�
=@���@��@���@��\@��+@�~�@�^5@�V@�-@���@��a@w33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B5?B<jB>wB@�B?}BA�BE�BS�BYBYB\)B[#BffBw�B�B�{B��B��B��B��B�JB��B�{B��B��B�qBɺB��B�B��BɺBĜBŢBɺBƨB�RB��B��B��B��B�!B�jB�?B��Bz�B[#BM�B49B{BB�B�#BȴB�jB�3B�!B��B��B�\B�1B�1B�7B�7B�Br�BG�B!�B�B&�B&�B�BuBB
�mB
��B
�B
p�B
bNB
XB
O�B
K�B
I�B
F�B
;dB
49B
,B
�B
VB
B	��B	�B	�B	ȴB	�jB	�RB	�?B	�-B	�B	��B	��B	�DB	|�B	o�B	^5B	M�B	K�B	F�B	@�B	8RB	8RB	C�B	O�B	K�B	H�B	M�B	N�B	E�B	B�B	A�B	>wB	8RB	)�B	�B	�B	�B	VB	+B	B	  B��B��B�B�`B�;B�#B�
B��B��BǮBB�wB�dB�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�bB�bB�VB�DB�+B�B�B|�Bz�Bw�Bu�Bu�Bu�Bu�Bs�Bp�Bm�Bk�BjBhsBgmBffBdZBbNB`BB_;B]/B[#BZBXBW
BVBT�BS�BQ�BQ�BP�BO�BQ�BS�BS�BO�BM�BO�BT�BVBVBYB]/B_;B_;B_;B`BB`BB]/BZBZBaHBe`BbNB]/B`BBZBR�BS�BYB[#B\)B]/B_;B`BB_;B]/BZBYBYB_;B`BB^5B_;B_;B_;BdZBffBgmBiyBk�Bp�Br�Bs�B�B�=B�DB�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�jBƨB��B��B�B�/B�TB�ZB�B�B�B�B��B	B	DB	VB	bB	hB	oB	�B	�B	�B	 �B	(�B	+B	+B	-B	2-B	5?B	7LB	9XB	<jB	>wB	@�B	A�B	B�B	C�B	C�B	D�B	J�B	J�B	J�B	L�B	N�B	Q�B	R�B	S�B	T�B	YB	YB	YB	YB	YB	XB	YB	\)B	aHB	bNB	e`B	gmB	k�B	o�B	o�B	o�B	o�B	r�B	s�B	u�B	v�B	v�B	y�B	z�B	{�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�B	�B	�%B	�+B	�=B	�=B	�=B	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�9B	�?B	�RB	�^B	�qB	�wB	�wB	��B	B	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ƨB	ƨB	ǮB	ȴA4~�B	�NB	�ZB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222  B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B5?B<jB>wB@�B?}BA�BE�BS�BYBYB\)B[#BffBw�B�B�{B��B��B��B��B�JB��B�{B��B��B�qBɺB��B�B��BɺBĜBŢBɺBƨB�RB��B��B��B��B�!B�jB�?B��Bz�B[#BM�B49B{BB�B�#BȴB�jB�3B�!B��B��B�\B�1B�1B�7B�7B�Br�BG�B!�B�B&�B&�B�BuBB
�mB
��B
�B
p�B
bNB
XB
O�B
K�B
I�B
F�B
;dB
49B
,B
�B
VB
B	��B	�B	�B	ȴB	�jB	�RB	�?B	�-B	�B	��B	��B	�DB	|�B	o�B	^5B	M�B	K�B	F�B	@�B	8RB	8RB	C�B	O�B	K�B	H�B	M�B	N�B	E�B	B�B	A�B	>wB	8RB	)�B	�B	�B	�B	VB	+B	B	  B��B��B�B�`B�;B�#B�
B��B��BǮBB�wB�dB�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�bB�bB�VB�DB�+B�B�B|�Bz�Bw�Bu�Bu�Bu�Bu�Bs�Bp�Bm�Bk�BjBhsBgmBffBdZBbNB`BB_;B]/B[#BZBXBW
BVBT�BS�BQ�BQ�BP�BO�BQ�BS�BS�BO�BM�BO�BT�BVBVBYB]/B_;B_;B_;B`BB`BB]/BZBZBaHBe`BbNB]/B`BBZBR�BS�BYB[#B\)B]/B_;B`BB_;B]/BZBYBYB_;B`BB^5B_;B_;B_;BdZBffBgmBiyBk�Bp�Br�Bs�B�B�=B�DB�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�jBƨB��B��B�B�/B�TB�ZB�B�B�B�B��B	B	DB	VB	bB	hB	oB	�B	�B	�B	 �B	(�B	+B	+B	-B	2-B	5?B	7LB	9XB	<jB	>wB	@�B	A�B	B�B	C�B	C�B	D�B	J�B	J�B	J�B	L�B	N�B	Q�B	R�B	S�B	T�B	YB	YB	YB	YB	YB	XB	YB	\)B	aHB	bNB	e`B	gmB	k�B	o�B	o�B	o�B	o�B	r�B	s�B	u�B	v�B	v�B	y�B	z�B	{�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�B	�B	�%B	�+B	�=B	�=B	�=B	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�9B	�?B	�RB	�^B	�qB	�wB	�wB	��B	B	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ƨB	ƨB	ǮB	ȴA4~�B	�NB	�ZB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191738                              AO  ARCAADJP                                                                    20181005191738    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191738  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191738  QCF$                G�O�G�O�G�O�8000            