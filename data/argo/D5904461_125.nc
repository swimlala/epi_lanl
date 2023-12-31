CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-22T19:17:52Z AOML 3.0 creation; 2016-08-07T21:36:47Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160622191752  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               }A   AO  5286_8897_125                   2C  D   APEX                            6531                            072314                          846 @׵�+�n�1   @׵��}2�@5U�$�/�c:ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    }A   B   B   @�33@�  A   AffA@  A`  A���A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D� D�<�D�� D��fD�3D�S3D���D�ٚD��D�P D��fD��fD�	�D�L�Dڃ3D� D�  D�0 D�c3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@\AG�A�AAG�AaG�A�p�A���A��
A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�C {C{C{C{C{C
{C{C.C.C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB.CD.CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
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
=C��pC��pC�
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
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�D��D�DD�DD�DD�DD�DD�D��D�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�Db��Dc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDteDy�D��D�?\D���D���D��D�U�D��\D��)D�\D�R�D���D���D�)D�O\Dڅ�D࢏D��D�2�D�e�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��
A��/A��HA��TA���A���Aͩ�A͏\A�x�A�?}A�7LA�33A�&�A��A�{A�JA�  A��;A̾wA̮A̛�ÃA�M�A�G�A�G�A�E�A� �A�A�A��A���A��/AƗ�AƅA�~�A�Q�A��A���AžwAš�A�z�A�K�A��A��A�1Aę�A�1'A���A�bNA�(�A�9XA���A�G�A���A�33A�bA��;A�Q�A��
A��A�JA�Q�A�r�A�5?A���A��A��A���A��9A��A��-A��A�dZA�A��!A�  A�9XA��yA�K�A��#A�jA�r�A���A�G�A��A��;A�ĜA��hA���A�~�A��^A��
A��-A�l�A��hA���A���A��A�33A���A�(�A�G�A�r�A���A�-A��A��`A�=qA���A��^A��PA~��A{&�Ax�9Aw��Awt�Au�hAq�^AnȴAk�7Ah�DAdbAb�AaS�A_/AY�wAW�
AT�AQ;dAN�AMXAK�AI�FAEdZAC
=AAA@�+A>~�A>�A=��A;�hA7��A5��A5S�A4M�A3��A2�HA1��A0��A.��A-��A,��A,bA,jA,�A,�A,I�A+;dA'�wA'A'
=A%��A$9XA#"�A"(�A ^5A�PA!|�A!/AbA/A��AA�7AXA��Av�A�A��A��A�A��A�jA��A�\A�A�;A^5A��AhsA�AffAA��AoA��AQ�At�A��Av�AM�A  A?}A
��A	/A�DA~�A��A��A�A�-A��A�A ~�A 9X@��
@�ff@���@�|�@�p�@��@��-@�ȴ@�ƨ@�O�@��`@��/@��`@��/@�r�@��;@�ƨ@�@��@��@�n�@��@�;d@�M�@��@�&�@�bN@�P@��@�7L@�
=@޸R@��#@���@�1@�@ڧ�@�=q@�-@���@�o@ڗ�@٩�@��@�bN@�S�@��@�l�@�t�@���@�\)@ѩ�@ϕ�@�|�@���@��\@��@���@�S�@��@���@��@��^@��/@�Q�@���@§�@�1'@¸R@š�@��@��
@��m@å�@��H@���@�p�@�9X@�~�@� �@�"�@��+@�-@�z�@��@�K�@�M�@���@�@��@���@�Ĝ@�%@�%@�%@��@���@��D@�j@�j@��;@�dZ@�ȴ@�v�@�$�@�J@���@���@��-@�`B@��`@�Q�@�1@���@���@��@�dZ@�~�@�5?@��@�@��#@���@��w@�`B@��j@��@�Ĝ@�bN@�(�@� �@�b@�  @�33@��@��@��\@�n�@�-@�$�@���@���@��h@��7@�X@�%@�%@�Ĝ@�(�@��w@��w@�9X@��@�+@��y@��\@�=q@��T@�`B@�&�@���@�j@��@��m@�dZ@�o@��@��\@�M�@���@�x�@��/@�z�@�9X@�1@�|�@��H@���@�^5@�E�@��T@��7@�x�@�X@�G�@�/@���@�bN@�9X@���@��H@�E�@��T@�?}@��j@���@�V@�`B@�7L@���@��@���@��9@���@�Ĝ@���@��@�z�@�9X@�  @�t�@�o@�n�@�$�@��@���@�x�@�X@��@���@��@��@�I�@��@��@��w@���@��P@�\)@�S�@�C�@�+@�o@��y@���@���@���@�=q@��T@�hs@�?}@��/@���@���@��@���@��m@��F@�dZ@�"�@�o@�
=@�@��@���@���@���@�ff@�-@���@��-@�x�@��@��@�j@�9X@��@�  @��
@�33@��!@�v�@�9X@�@wl�@qx�@iX@_�;@X  @Pb@I�#@E�h@?�@8Ĝ@0  @+o@%��@ r�@33@;d@t�@��@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A��
A��/A��HA��TA���A���Aͩ�A͏\A�x�A�?}A�7LA�33A�&�A��A�{A�JA�  A��;A̾wA̮A̛�ÃA�M�A�G�A�G�A�E�A� �A�A�A��A���A��/AƗ�AƅA�~�A�Q�A��A���AžwAš�A�z�A�K�A��A��A�1Aę�A�1'A���A�bNA�(�A�9XA���A�G�A���A�33A�bA��;A�Q�A��
A��A�JA�Q�A�r�A�5?A���A��A��A���A��9A��A��-A��A�dZA�A��!A�  A�9XA��yA�K�A��#A�jA�r�A���A�G�A��A��;A�ĜA��hA���A�~�A��^A��
A��-A�l�A��hA���A���A��A�33A���A�(�A�G�A�r�A���A�-A��A��`A�=qA���A��^A��PA~��A{&�Ax�9Aw��Awt�Au�hAq�^AnȴAk�7Ah�DAdbAb�AaS�A_/AY�wAW�
AT�AQ;dAN�AMXAK�AI�FAEdZAC
=AAA@�+A>~�A>�A=��A;�hA7��A5��A5S�A4M�A3��A2�HA1��A0��A.��A-��A,��A,bA,jA,�A,�A,I�A+;dA'�wA'A'
=A%��A$9XA#"�A"(�A ^5A�PA!|�A!/AbA/A��AA�7AXA��Av�A�A��A��A�A��A�jA��A�\A�A�;A^5A��AhsA�AffAA��AoA��AQ�At�A��Av�AM�A  A?}A
��A	/A�DA~�A��A��A�A�-A��A�A ~�A 9X@��
@�ff@���@�|�@�p�@��@��-@�ȴ@�ƨ@�O�@��`@��/@��`@��/@�r�@��;@�ƨ@�@��@��@�n�@��@�;d@�M�@��@�&�@�bN@�P@��@�7L@�
=@޸R@��#@���@�1@�@ڧ�@�=q@�-@���@�o@ڗ�@٩�@��@�bN@�S�@��@�l�@�t�@���@�\)@ѩ�@ϕ�@�|�@���@��\@��@���@�S�@��@���@��@��^@��/@�Q�@���@§�@�1'@¸R@š�@��@��
@��m@å�@��H@���@�p�@�9X@�~�@� �@�"�@��+@�-@�z�@��@�K�@�M�@���@�@��@���@�Ĝ@�%@�%@�%@��@���@��D@�j@�j@��;@�dZ@�ȴ@�v�@�$�@�J@���@���@��-@�`B@��`@�Q�@�1@���@���@��@�dZ@�~�@�5?@��@�@��#@���@��w@�`B@��j@��@�Ĝ@�bN@�(�@� �@�b@�  @�33@��@��@��\@�n�@�-@�$�@���@���@��h@��7@�X@�%@�%@�Ĝ@�(�@��w@��w@�9X@��@�+@��y@��\@�=q@��T@�`B@�&�@���@�j@��@��m@�dZ@�o@��@��\@�M�@���@�x�@��/@�z�@�9X@�1@�|�@��H@���@�^5@�E�@��T@��7@�x�@�X@�G�@�/@���@�bN@�9X@���@��H@�E�@��T@�?}@��j@���@�V@�`B@�7L@���@��@���@��9@���@�Ĝ@���@��@�z�@�9X@�  @�t�@�o@�n�@�$�@��@���@�x�@�X@��@���@��@��@�I�@��@��@��w@���@��P@�\)@�S�@�C�@�+@�o@��y@���@���@���@�=q@��T@�hs@�?}@��/@���@���@��@���@��m@��F@�dZ@�"�@�o@�
=@�@��@���@���@���@�ff@�-@���@��-@�x�@��@��@�j@�9X@��@�  @��
@�33@��!G�O�@�9X@�@wl�@qx�@iX@_�;@X  @Pb@I�#@E�h@?�@8Ĝ@0  @+o@%��@ r�@33@;d@t�@��@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�wB	�wB	�wB	�wB	�wB	�qB	�qB	�qB	�jB	�jB	�dB	�dB	�dB	�^B	�^B	�^B	�XB	�XB	�RB	�LB	�LB	�FB	�FB	�FB	�RB	�XB	�RB	�RB	�RB	ɺB	�TB	��B
{B
�B
�B
�B
hB
bB
{B
{B
�B
�B
"�B
!�B
#�B
%�B
+B
+B
0!B
J�B
\)B
�=B
�wB
ɺB
�B�B[#BjBo�Bk�Be`B]/BVBaHBhsB� B�uB��B�B��B�5B�ZB�ZB�B�B�B��BVBPB%B��B��B��B�B��B��B�9B��B��B�VB�Bu�Bm�BdZBO�B@�B�B
��B
�bB
~�B
t�B
YB
?}B
�B
PB
B	�ZB	�-B	�\B	q�B	\)B	O�B	?}B	33B	0!B	'�B	@�B	P�B	5?B	!�B	hB��B�B�sB�;BɺB�B��B�\B~�B� B�1B�1By�BhsBcTBbNBdZBk�Bn�Br�Br�Bu�B|�B{�Bx�Bx�B{�By�B�B�B�B�B�B�JB��B��B��B�!B�B�B�9B�?B�'B��B��B�uB��BǮB��BŢB��B�LB�LB�}B��B�ZB	B	uB	�B	!�B	?}B	XB	[#B	S�B	I�B	33B	�B	bB	JB		7B	%B	B	B	B	B	B	B	%B	VB	bB	\B	JB	1B	B��B��B��B�B�B�;B��B��B��B��B��B��B��BȴBŢBBBĜB��B�dB�FB�LB�FB�FB�FB�FB�LB�LB�LB�FB�FB�FB�RB�qB�wB�}B��B��B��B�}B�wB�jB�jB�jBBŢBĜBŢBƨB��B�5B�sB�B�B��B��B��B�B�B�B��B��B�B�B�BĜB�?BƨB�)B�sB�B	B	B��B��B	B		7B	{B	�B	�B	.B	49B	5?B	5?B	5?B	33B	49B	33B	1'B	-B	'�B	&�B	&�B	%�B	"�B	#�B	%�B	(�B	(�B	+B	-B	1'B	9XB	?}B	@�B	@�B	@�B	A�B	B�B	B�B	D�B	G�B	I�B	M�B	P�B	R�B	T�B	VB	W
B	XB	ZB	[#B	[#B	[#B	]/B	^5B	]/B	]/B	^5B	`BB	cTB	e`B	e`B	dZB	cTB	cTB	dZB	gmB	l�B	m�B	n�B	n�B	n�B	q�B	r�B	r�B	r�B	u�B	v�B	y�B	{�B	~�B	�B	�B	�+B	�1B	�DB	�JB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�FB	�RB	�XB	�dB	�qB	�wB	��B	B	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ǮB	ǮB	ŢB	ĜB	ĜB	ÖB	B	B	ŢB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
JB
{B
�B
 �B
(�B
1'B
8RB
?}B
H�B
K�B
R�B
W
B
\)B
^5B
`BB
ffB
jB
n�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�~B	�~B	�}B	�{B	�}B	�wB	�tB	�wB	�nB	�oB	�kB	�jB	�jB	�cB	�dB	�cB	�\B	�_B	�YB	�PB	�QB	�KB	�JB	�JB	�VB	�]B	�VB	�XB	�WB	ɿB	�\B	��B
}B
�B
�B
�B
iB
bB
yB
{B
�B
�B
"�B
!�B
#�B
%�B
+B
+B
0#B
J�B
\(B
�;B
�vB
ɸB
�B�B[Bj{Bo�Bk�BeZB]'BU�BaCBhmB�B�nB��B��B��B�/B�RB�RB�B�B�B��BOBIBB��B��B��B�B��B��B�2B��B�wB�RB� Bu�Bm�BdQBO�B@}B�B
��B
�\B
~�B
t�B
YB
?zB
�B
PB
B	�[B	�0B	�aB	q�B	\1B	O�B	?�B	39B	0*B	'�B	@�B	P�B	5IB	!�B	rB��B�B�B�FB��B�B��B�kB	B�B�CB�=By�Bh�BceBb[BdhBk�Bn�Br�Br�Bu�B|�B{�Bx�Bx�B{�By�B�%B�&B�B�B�B�VB��B��B��B�,B�B�B�EB�IB�1B�	B��B�B��BǷB�BŪB��B�VB�TB��B��B�bB	$B	{B	�B	!�B	?�B	XB	[&B	S�B	I�B	37B	�B	jB	OB		@B	)B	B	 B	B	B	B	B	*B	]B	iB	aB	PB	6B	 B��B��B��B�B�B�EB��B��B��B��B��B��B��BȽBūBBBĥB��B�nB�NB�TB�PB�PB�PB�OB�TB�SB�UB�OB�PB�OB�[B�zB��B��B��B��B��B��B��B�qB�sB�rBBŪBĦBŨBƯB��B�<B�zB�B�B��B��B��B�B�B�B��B��B�B�B�%BĦB�IBƱB�/B�wB�B	B	B��B��B	B		>B	�B	�B	�B	.B	4=B	5BB	5AB	5CB	37B	4=B	35B	1*B	-B	'�B	&�B	&�B	%�B	"�B	#�B	%�B	(�B	(�B	+B	-B	1(B	9ZB	?B	@�B	@�B	@�B	A�B	B�B	B�B	D�B	G�B	I�B	M�B	P�B	R�B	T�B	VB	WB	XB	ZB	[#B	[&B	[#B	]0B	^6B	]/B	]0B	^7B	`CB	cVB	eaB	ebB	d\B	cTB	cUB	dZB	gmB	l�B	m�B	n�B	n�B	n�B	q�B	r�B	r�B	r�B	u�B	v�B	y�B	{�B	~�B	�B	�B	�*B	�1B	�CB	�LB	�\B	�]B	�gB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	� B	�-B	�7B	�DB	�DB	�OB	�UB	�bB	�oB	�tB	��B	B	ęB	ĜB	ĜB	ĘB	ĚB	ƤB	ǭB	ǬB	šB	ěB	ěB	ÒB	B	B	ŠB	ɺB	ʽB	��B	��B	��B	��B	��B	� B	�B	�B	� B	�!B	�%B	�&B	�,B	�2B	�9B	�9B	�?B	�>B	�EB	�KB	�WB	�VB	�VB	�\B	�]B	�^B	�_B	�aB	�aB	�jB	�iB	�mB	�pB	�pB	�wB	�wB	�tB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
FB
wB
�B
 �B
(�B
1!B
8NB
?xB
H�B
K�B
R�B
WB
\!B
^1B
`;B
fbB
jxB
n�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.08 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436482016080714364820160807143648  AO  ARCAADJP                                                                    20160622191752    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160622191752  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160622191752  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143648  IP                  G�O�G�O�G�O�                