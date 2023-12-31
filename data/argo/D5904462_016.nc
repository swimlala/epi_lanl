CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:16Z AOML 3.0 creation; 2016-08-07T21:51:11Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221416  20160807145111  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_016                   2C  D   APEX                            6529                            072314                          846 @�!Dכ�1   @�!El��@2�G�z��c�G�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�33B�33B�  B���B�  B�  B���B�  B�  B�33B�  B���B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyY�D�fD�FfD�p D��fD���D�C3D�y�D��fD��D�I�D��3D�ɚD�  D�9�Dڃ3D�� D� D�33D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @\(�@�{@�{A
=A'
=AG
=Ag
=A��A��A��A��AÅAӅA�A�BB	BBB!B)B1B9BABIBQBYBaBiBqByB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�z�B��B��B��HB��HB��HB�{B�{B��HBخB��HB��HB�B��HB��HB�{B��HB��B��C p�Cp�Cp�Cp�Cp�C
p�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:p�C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cbp�Cd�>Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D��D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�\Dyu�D�$zD�TzD�~D��zD�
�D�QGD���D��zD��D�W�D��GD�׮D�D�G�DڑGD��D�D�AGD�z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AہAۃAۅAۇ+AۋDAۉ7AۃA�z�A�n�A�t�A�r�A�n�A�ffA�Q�A�A�A�;dA�33A�/A�$�A�"�A� �A�"�A���A��A��mA��A��A�{A�&�A�&�A�(�A�{A���A�1A�JA�oA�9XA�VA�ĜA�p�A�E�A�A�A�E�A�XA�r�A�1A�  A�5?Aӟ�AѶFA�ƨAϥ�A�1A͟�A���A��A��A��mA�+A�-A��A�33A��wA�O�A�5?A�JA��A���A��A�JA���A�\)A��A��^A�ffA�\)A�z�A��/A��+A�=qA���A�7LA���A�JA�r�A��jA���A��A��jA�  A� �A�5?A���A�&�A���A��7A�S�A��TA�XA�z�A��7A�/A�&�A�p�A�`BA��A��7A�ȴA��A��^Az �Aw�-Av��AvE�Aq��Al��Ai��AfJA_t�A\ZAZ��AX��AW�hAU�AS��AR5?AMAJ��AH��AHVAG�^AEhsACp�AA�A@�!A@^5A?O�A>5?A<�`A<=qA<��A;7LA9dZA7�#A6��A3�A1��A0�A0bA/�A/C�A.��A,��A,  A+%A*n�A)��A(�/A'hsA&^5A#��A"jA!p�A��A9XAO�A�^A��AI�A"�A�A�A�A�TA�AS�AI�A`BAffA\)A�`A�uAZA�mA\)AȴA�DA�A\)AXAXAp�AhsA7LA�yA�AVA�mA�A
5?A	�^A	p�A�!A5?A��A&�AĜA  A��A"�A�+A  A�7A7LA�A��AjA  AXA ��A n�A   @��T@�&�@��@� �@�V@�Ĝ@�Ĝ@���@�@�E�@��@�@�`B@��@�9X@��P@��+@���@�Z@��H@�M�@�&�@�b@�F@�
=@���@�5?@�/@�D@�P@�{@�@�G�@�%@�b@�1@�{@�/@�"�@��H@��@�S�@�"�@�X@��@���@�j@�@��`@���@߮@߮@ߝ�@ߍP@�l�@�\)@�\)@�
=@�~�@��#@ݡ�@�hs@�7L@���@��`@���@ܼj@�j@���@�dZ@ڇ+@��#@ف@�hs@��@��@���@�z�@���@���@׶F@�C�@֏\@�$�@�@��#@�@Չ7@�`B@��@���@�r�@�
=@�5?@��@���@ѡ�@�/@мj@�A�@���@υ@�S�@�\)@�\)@�33@�+@�@�{@�p�@��`@̛�@�z�@�bN@�A�@�1'@�t�@ʗ�@�O�@�O�@�/@ȼj@�(�@��m@��@���@��@��
@�C�@�~�@�M�@�5?@��@�J@��T@�@őh@��@�j@öF@�33@��y@�=q@���@�?}@���@���@��R@��!@�$�@���@�Ĝ@��u@���@��@�Q�@���@�v�@���@��@�&�@� �@�+@�{@��^@�7L@��@��@���@�33@���@�ff@�E�@�M�@��@��#@�hs@�O�@�/@�%@��@�9X@���@��y@��+@�{@��h@��@�(�@�ƨ@��@�l�@�;d@�@���@��R@�J@�@��7@��@�9X@�S�@���@��\@�5?@��@�{@���@��-@�?}@��j@�r�@�A�@�b@�b@�  @��;@��w@���@��@�dZ@�"�@���@�M�@�@��#@�@��h@�&�@���@��9@���@�9X@��P@�l�@�S�@�33@�
=@��@��R@�v�@��@��h@��@���@�Ĝ@���@��@�|�@�o@�^5@�{@�@�G�@���@���@���@�1@��F@��@���@�|�@�K�@�"�@�o@�@��R@��j@���@�t�@��y@{"�@sS�@jJ@` �@W+@M@H �@Ahs@9��@3o@,�@%p�@V@bN@dZ@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   AہAۃAۅAۇ+AۋDAۉ7AۃA�z�A�n�A�t�A�r�A�n�A�ffA�Q�A�A�A�;dA�33A�/A�$�A�"�A� �A�"�A���A��A��mA��A��A�{A�&�A�&�A�(�A�{A���A�1A�JA�oA�9XA�VA�ĜA�p�A�E�A�A�A�E�A�XA�r�A�1A�  A�5?Aӟ�AѶFA�ƨAϥ�A�1A͟�A���A��A��A��mA�+A�-A��A�33A��wA�O�A�5?A�JA��A���A��A�JA���A�\)A��A��^A�ffA�\)A�z�A��/A��+A�=qA���A�7LA���A�JA�r�A��jA���A��A��jA�  A� �A�5?A���A�&�A���A��7A�S�A��TA�XA�z�A��7A�/A�&�A�p�A�`BA��A��7A�ȴA��A��^Az �Aw�-Av��AvE�Aq��Al��Ai��AfJA_t�A\ZAZ��AX��AW�hAU�AS��AR5?AMAJ��AH��AHVAG�^AEhsACp�AA�A@�!A@^5A?O�A>5?A<�`A<=qA<��A;7LA9dZA7�#A6��A3�A1��A0�A0bA/�A/C�A.��A,��A,  A+%A*n�A)��A(�/A'hsA&^5A#��A"jA!p�A��A9XAO�A�^A��AI�A"�A�A�A�A�TA�AS�AI�A`BAffA\)A�`A�uAZA�mA\)AȴA�DA�A\)AXAXAp�AhsA7LA�yA�AVA�mA�A
5?A	�^A	p�A�!A5?A��A&�AĜA  A��A"�A�+A  A�7A7LA�A��AjA  AXA ��A n�A   @��T@�&�@��@� �@�V@�Ĝ@�Ĝ@���@�@�E�@��@�@�`B@��@�9X@��P@��+@���@�Z@��H@�M�@�&�@�b@�F@�
=@���@�5?@�/@�D@�P@�{@�@�G�@�%@�b@�1@�{@�/@�"�@��H@��@�S�@�"�@�X@��@���@�j@�@��`@���@߮@߮@ߝ�@ߍP@�l�@�\)@�\)@�
=@�~�@��#@ݡ�@�hs@�7L@���@��`@���@ܼj@�j@���@�dZ@ڇ+@��#@ف@�hs@��@��@���@�z�@���@���@׶F@�C�@֏\@�$�@�@��#@�@Չ7@�`B@��@���@�r�@�
=@�5?@��@���@ѡ�@�/@мj@�A�@���@υ@�S�@�\)@�\)@�33@�+@�@�{@�p�@��`@̛�@�z�@�bN@�A�@�1'@�t�@ʗ�@�O�@�O�@�/@ȼj@�(�@��m@��@���@��@��
@�C�@�~�@�M�@�5?@��@�J@��T@�@őh@��@�j@öF@�33@��y@�=q@���@�?}@���@���@��R@��!@�$�@���@�Ĝ@��u@���@��@�Q�@���@�v�@���@��@�&�@� �@�+@�{@��^@�7L@��@��@���@�33@���@�ff@�E�@�M�@��@��#@�hs@�O�@�/@�%@��@�9X@���@��y@��+@�{@��h@��@�(�@�ƨ@��@�l�@�;d@�@���@��R@�J@�@��7@��@�9X@�S�@���@��\@�5?@��@�{@���@��-@�?}@��j@�r�@�A�@�b@�b@�  @��;@��w@���@��@�dZ@�"�@���@�M�@�@��#@�@��h@�&�@���@��9@���@�9X@��P@�l�@�S�@�33@�
=@��@��R@�v�@��@��h@��@���@�Ĝ@���@��@�|�@�o@�^5@�{@�@�G�@���@���@���@�1@��F@��@���@�|�@�K�@�"�@�o@�G�O�@��j@���@�t�@��y@{"�@sS�@jJ@` �@W+@M@H �@Ahs@9��@3o@,�@%p�@V@bN@dZ@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�+B�+B�1B�1B�1B�1B�+B�%B�%B�%B�%B�+B�DB�bB�\B�VB�PB�VB�JB�JB�JB�JB�%B�B�B�B�+B�hB��B��B��B��B��B��B��B��B�B�}BĜB�wB�jB�wBB��B�B��BB\B!�B>wBH�BQ�BcTBe`B�B��B��B�'B�dB��B�B�#B��B��B��B�B�B�B��B��B�wB�^B�RB�^B�qBƨB��B��B��B��BȴBŢB�}B�!B��B��B�JB�Br�BYB;dB�BB�\Bo�BW
B?}B�B
�B
�TB
�B
�jB
�B
�bB
r�B
ffB
T�B
;dB
+B
hB	��B	�jB	�?B	�B	�hB	q�B	^5B	I�B	(�B	�B	bB		7B	B��B��B�B�fB�HB�NB�5B�#B�
B�
B�B�B�/B�)B�#B�#B�HB�B��B�NB��B�#B��B��B�B�;B�)B�#B�/B��B��B	B	B	B��B��B��B�B��B�B�B�B�B�`B�NB�HB�BB�TB�ZB�fB�sB�sB�B�B�B�B��B��B��B	B	oB	oB	bB	�B	�B	 �B	"�B	"�B	(�B	.B	33B	6FB	8RB	9XB	;dB	>wB	A�B	D�B	E�B	G�B	J�B	I�B	N�B	M�B	H�B	E�B	D�B	D�B	I�B	L�B	M�B	M�B	M�B	M�B	K�B	K�B	L�B	L�B	K�B	H�B	H�B	G�B	R�B	N�B	N�B	P�B	T�B	e`B	p�B	r�B	s�B	x�B	y�B	{�B	|�B	|�B	|�B	~�B	|�B	y�B	x�B	x�B	~�B	�B	�7B	�+B	�B	�B	�B	~�B	� B	� B	�B	�+B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�{B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�RB	�^B	�dB	�qB	�qB	�wB	�}B	�}B	�}B	�}B	�}B	��B	��B	��B	�}B	��B	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�
B	�
B	�
B	��B	�
B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�#B	�;B	�HB	�BB	�BB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�ZB	�ZB	�ZB	�fB	�`B	�mB	�fB	�fB	�`B	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
	7B
bB
�B
�B
&�B
.B
6FB
=qB
?}B
F�B
K�B
Q�B
XB
]/B
bNB
hsB
m�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B�B�
B�B�B�B�B�B�B�B�B�B�
B� B�@B�;B�6B�-B�4B�(B�(B�(B�(B�B��B��B��B�B�EB�iB�kB�wB�wB�iB�~B��B��B��B�\B�{B�TB�HB�TB�nBʞB��B��B �B8B!�B>TBH�BQ�Bc1Be;B��B�vB��B�B�=B̨B��B��B��B��B��B�B�~B�]B��B�cB�UB�:B�/B�8B�LBƂBˠBϻBβB̤BȏB�|B�XB��B��B�|B�!B��Br�BX�B;;B�B�hB�1BouBV�B?VB�B
�B
�.B
��B
�DB
��B
�;B
r�B
f@B
T�B
;?B
*�B
@B	��B	�DB	�B	��B	�GB	q�B	^B	I�B	(�B	sB	DB		B	�B��B��B�sB�HB�)B�1B�B�B��B��B��B�B�B�
B�B�B�)B��B��B�0B��B�B��B��B��B�B�	B�B�B��B��B	 �B	�B	�B��B��B��B�B��B�B�|B�jB�\B�:B�,B�%B�!B�1B�9B�EB�OB�OB�]B��B�B�B��B��B��B	�B	HB	JB	<B	gB	�B	 �B	"�B	"�B	(�B	-�B	3B	6B	8,B	90B	;=B	>OB	AbB	DwB	E|B	G�B	J�B	I�B	N�B	M�B	H�B	E|B	DsB	DtB	I�B	L�B	M�B	M�B	M�B	M�B	K�B	K�B	L�B	L�B	K�B	H�B	H�B	G�B	R�B	N�B	N�B	P�B	T�B	e6B	pzB	r�B	s�B	x�B	y�B	{�B	|�B	|�B	|�B	~�B	|�B	y�B	x�B	x�B	~�B	��B	�B	��B	��B	��B	��B	~�B	�B	�B	��B	��B	�B	��B	��B	�eB	�lB	��B	��B	��B	��B	�WB	�;B	�PB	�mB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�0B	�8B	�CB	�@B	�HB	�MB	�KB	�PB	�NB	�QB	�VB	�\B	�ZB	�LB	�YB	�nB	�kB	�lB	�lB	�lB	�tB	�{B	�xB	�tB	�tB	�tB	�zB	�xB	�wB	�xB	�B	�B	ȅB	ɎB	ʑB	ɊB	ʐB	ʓB	ʔB	˘B	̟B	ϰB	жB	ѽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�+B	�*B	�%B	�+B	�(B	�+B	�,B	�*B	�(B	�'B	�5B	�1B	�;B	�7B	�4B	�0B	�1B	�5B	�6B	�=B	�GB	�TB	�TB	�TB	�[B	�ZB	�ZB	�XB	�YB	�XB	�FB	�@B	�CB	�BB	�CB	�HB	�UB	�TB	�RB	�YB	�aB	�gB	�bB	�eB	�iB	�gB	�eB	�hB	�eB	�lB	�nB	�kB	�lB	�jB	�lB	�lB	�kB	�mB	�qB	�rB	�rB	�sB	�qB	�tB	�qB	�zB	�xB	�yB	�zB	�|B	�xB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
	B
/B
RB
zB
&�B
-�B
6B
==B
?HB
FuB
K�B
Q�B
W�B
\�B
bB
h>B
m[B
r|B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451112016080714511120160807145111  AO  ARCAADJP                                                                    20150226221416    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221416  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221416  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145111  IP                  G�O�G�O�G�O�                