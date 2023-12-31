CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-12T19:34:42Z AOML 3.0 creation; 2016-08-07T21:51:15Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150312193442  20160807145115  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               (A   AO  5287_9017_040                   2C  D   APEX                            6529                            072314                          846 @�@�hN�1   @�@���@1��-�d�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    (A   B   B   @���@�  A   A   AA��Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�  D�L�D�y�D�� D� D�33D���D�� D��fD�VfD��fD��3D� D�FfDڌ�D��fD��D�P D�fD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�{A
=A'
=AH��Ah��A��A��A��A��AÅAӅA�A�BB	BB\)B!B)B1B9BABIBQBYBaBiBqByB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC p�Cp�Cp�Cp�Cp�C
p�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:p�C<�>C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cbp�Cdp�Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�EC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>��D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ�DZ��D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Du�Dy��D�D�Z�D���D��D�D�AGD���D��D�zD�dzD��zD��GD�D�TzDښ�D��zD�*�D�^D�zD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aˏ\Aˏ\A˙�A˗�A˙�A˙�AˑhAˏ\AˋDAˉ7AˍPAˏ\A˗�A˗�Aˡ�A˥�A˥�A˥�A˧�A˰!A˰!A˰!A˲-A˴9A˶FA˶FA˸RA˸RA˶FA˶FA˴9A˺^A�ƨA���A��A��TA��TA��`A��yA��A��A���A�33A�t�A�+AɾwA�=qA���AȬA���A�
=A���A�bNA�%AľwAď\A�I�A��Aò-AÅA�r�A�K�A���A�A�33A��A�ZA�(�A�33A�A�dZA��A��A��A��A��A�A�ƨA�A��wA���A�+A��A�ƨA�{A�=qA�7LA�XA�VA� �A�1'A��A�oA���A��A�A��HA�33A���A�z�A�;dA�JA�VA�hsA�  A�XA�A�A�/A���A�A{�^Ay�7AyC�Ax��At�uAlZAg�Ac�A`�A_t�A]A\��A[K�AY��AWAT��AS��ARQ�AO�AL�RAKO�AJ�AI33AH��AG��AGhsAE�ADZAC7LA?�A>ȴA>ffA>JA<��A<I�A;�wA:�\A8-A6��A4  A2JA01'A/hsA-hsA+�#A*ZA)7LA'33A&�A%x�A%�A$JA ��A�A��AZA�AI�A/A��AƨA�+AA33A�RA��A��A�A�A�yA{AE�A/A
�!A	��A	|�A��A5?AA��A�9@��@��
@�;d@�X@���@�ff@��T@�V@���@�\)@�v�@��D@��@�j@���@�  @�R@�h@�X@��@�R@�J@���@��@�^5@�hs@�@�P@�"�@�@旍@�@䛦@�ȴ@�E�@��#@ᙚ@�O�@���@�~�@��@ݡ�@���@�  @�+@���@��y@ۍP@���@��@�x�@�X@ָR@���@�1@ա�@�x�@�dZ@Ѻ^@�G�@�j@�K�@�"�@Ώ\@�-@���@͙�@�?}@�{@ͺ^@�?}@�9X@���@��;@˶F@��y@�M�@��#@ɩ�@ȋD@ǥ�@Ɵ�@�Z@¸R@�x�@�7L@�%@�Ĝ@���@��;@���@�dZ@�"�@�ff@���@�7L@�Ĝ@��/@��h@�O�@��@��9@�Q�@�b@�  @��;@�t�@�33@���@�-@��@�@�@�x�@��@���@���@�z�@�r�@�z�@��@��`@��@��m@���@�|�@�+@�"�@�"�@�@���@�n�@�@�ff@�M�@���@�@�O�@���@���@� �@��j@��@��@���@��-@�&�@��@�I�@�  @�K�@��y@�ȴ@��\@��-@�%@��u@�Q�@��@�1@���@�dZ@�;d@�+@��y@�E�@���@��h@���@�9X@��H@�n�@�{@�@��T@���@�7L@��u@�9X@�1'@��@�ƨ@���@�|�@�\)@�K�@�+@��@��!@�$�@��@���@���@�O�@��@�bN@�b@��F@�+@��H@���@�M�@�{@��#@��^@���@��h@�p�@�O�@��@��@��/@��9@�j@�A�@���@��F@���@��P@�dZ@�K�@�;d@�"�@��@�^5@�=q@�{@��T@�@���@��7@�hs@�G�@��@�Ĝ@�I�@��m@��F@�33@�M�@�-@��@��-@���@��@�X@��@��@�Ĝ@���@���@��@�j@�(�@���@�\)@�;d@��@���@�ff@�M�@��@���@��7@�7L@���@�bN@� �@��;@��F@��@�K�@���@�v�@�$�@�@�hs@�7L@�&�@���@��9@���@��@��@��
@��F@���@�|�@�;d@�ȴ@�v�@�5?@�5?@�@�/@��j@��j@��9@��@�x�@|�@x�`@q�7@j�H@d1@[��@SS�@G�@?�P@6V@0Q�@+t�@%��@ 1'@t�@@��@@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aˏ\Aˏ\A˙�A˗�A˙�A˙�AˑhAˏ\AˋDAˉ7AˍPAˏ\A˗�A˗�Aˡ�A˥�A˥�A˥�A˧�A˰!A˰!A˰!A˲-A˴9A˶FA˶FA˸RA˸RA˶FA˶FA˴9A˺^A�ƨA���A��A��TA��TA��`A��yA��A��A���A�33A�t�A�+AɾwA�=qA���AȬA���A�
=A���A�bNA�%AľwAď\A�I�A��Aò-AÅA�r�A�K�A���A�A�33A��A�ZA�(�A�33A�A�dZA��A��A��A��A��A�A�ƨA�A��wA���A�+A��A�ƨA�{A�=qA�7LA�XA�VA� �A�1'A��A�oA���A��A�A��HA�33A���A�z�A�;dA�JA�VA�hsA�  A�XA�A�A�/A���A�A{�^Ay�7AyC�Ax��At�uAlZAg�Ac�A`�A_t�A]A\��A[K�AY��AWAT��AS��ARQ�AO�AL�RAKO�AJ�AI33AH��AG��AGhsAE�ADZAC7LA?�A>ȴA>ffA>JA<��A<I�A;�wA:�\A8-A6��A4  A2JA01'A/hsA-hsA+�#A*ZA)7LA'33A&�A%x�A%�A$JA ��A�A��AZA�AI�A/A��AƨA�+AA33A�RA��A��A�A�A�yA{AE�A/A
�!A	��A	|�A��A5?AA��A�9@��@��
@�;d@�X@���@�ff@��T@�V@���@�\)@�v�@��D@��@�j@���@�  @�R@�h@�X@��@�R@�J@���@��@�^5@�hs@�@�P@�"�@�@旍@�@䛦@�ȴ@�E�@��#@ᙚ@�O�@���@�~�@��@ݡ�@���@�  @�+@���@��y@ۍP@���@��@�x�@�X@ָR@���@�1@ա�@�x�@�dZ@Ѻ^@�G�@�j@�K�@�"�@Ώ\@�-@���@͙�@�?}@�{@ͺ^@�?}@�9X@���@��;@˶F@��y@�M�@��#@ɩ�@ȋD@ǥ�@Ɵ�@�Z@¸R@�x�@�7L@�%@�Ĝ@���@��;@���@�dZ@�"�@�ff@���@�7L@�Ĝ@��/@��h@�O�@��@��9@�Q�@�b@�  @��;@�t�@�33@���@�-@��@�@�@�x�@��@���@���@�z�@�r�@�z�@��@��`@��@��m@���@�|�@�+@�"�@�"�@�@���@�n�@�@�ff@�M�@���@�@�O�@���@���@� �@��j@��@��@���@��-@�&�@��@�I�@�  @�K�@��y@�ȴ@��\@��-@�%@��u@�Q�@��@�1@���@�dZ@�;d@�+@��y@�E�@���@��h@���@�9X@��H@�n�@�{@�@��T@���@�7L@��u@�9X@�1'@��@�ƨ@���@�|�@�\)@�K�@�+@��@��!@�$�@��@���@���@�O�@��@�bN@�b@��F@�+@��H@���@�M�@�{@��#@��^@���@��h@�p�@�O�@��@��@��/@��9@�j@�A�@���@��F@���@��P@�dZ@�K�@�;d@�"�@��@�^5@�=q@�{@��T@�@���@��7@�hs@�G�@��@�Ĝ@�I�@��m@��F@�33@�M�@�-@��@��-@���@��@�X@��@��@�Ĝ@���@���@��@�j@�(�@���@�\)@�;d@��@���@�ff@�M�@��@���@��7@�7L@���@�bN@� �@��;@��F@��@�K�@���@�v�@�$�@�@�hs@�7L@�&�@���@��9@���@��@��@��
@��F@���@�|�@�;d@�ȴ@�v�@�5?@�5?@�@�/@��j@��jG�O�@��@�x�@|�@x�`@q�7@j�H@d1@[��@SS�@G�@?�P@6V@0Q�@+t�@%��@ 1'@t�@@��@@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBe`Be`Be`Be`Be`Be`BdZBdZBdZBe`BdZBdZBdZBdZBdZBdZBdZBdZBdZBe`Be`Be`Be`Be`BffBffBgmBffBffBffBffBhsBm�Br�Bz�B�1B�=B�JB�VB�VB��B�qB�B8RBD�Bk�B�+B�DB�{B�dB��B�B  BBBDBoB�B�B�B�B(�B>wBO�BR�BK�BM�BL�BN�BYBZB_;BjBo�Br�Bn�B[#BG�B��B�B�B��B�NB��B�!B�B��B}�B��B$�B�B�1Bl�BbNBH�B'�B1B
�B
��B
B
�XB
�LB
��B
��B
{�B
_;B
M�B
=qB
'�B
PB	�yB	�#B	�
B	��B	�9B	�B	gmB	W
B	H�B	@�B	:^B	5?B	1'B	/B	%�B	&�B	$�B	�B	
=B��B��B�B�B�mB�NB�;B�
B��B��B��BǮBƨBŢBŢBĜBB��B��B��BĜBĜBĜBĜBŢBŢBŢBŢBȴBǮBĜB��B�jB�FB�9B�FB�FB�RB�jB�dB�XB�XB�^B�^B�FB�LB�^B�XB�RB�FB�9B�B�B��B��B��B��B��B��B��B�PBn�BP�BA�B>wB9XBA�B5?B:^BB�BK�B?}B@�B=qBE�BI�B\)Bm�Bn�Bs�Bw�B�B�B�B�1B�DB�DB�\B�hB�uB�uB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�?B�dB�wBǮB�}B�^B�}B��B�B��B��B��B�
B�#B�5B�NB�sB�B�B�B��B	B	+B	
=B	JB	bB	oB	�B	�B	�B	�B	�B	!�B	 �B	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	&�B	+B	0!B	33B	6FB	8RB	=qB	H�B	M�B	P�B	S�B	W
B	YB	ZB	[#B	^5B	aHB	e`B	iyB	jB	l�B	m�B	q�B	t�B	v�B	y�B	}�B	�B	�B	�%B	�DB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�LB	�RB	�XB	�dB	�qB	�}B	��B	ÖB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ǮB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�BB	�HB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B

=B

=B
DB
JB
PB
\B
oB
�B
�B
�B
&�B
,B
49B
7LB
<jB
D�B
K�B
R�B
ZB
_;B
e`B
l�B
p�B
s�B
u�B
y�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Be>Be<Be>Be;Be>Be;Bd5Bd7Bd4Be>Bd4Bd7Bd8Bd8Bd5Bd5Bd5Bd5Bd5BeABeABe<Be>Be>BfDBfDBgLBfABfDBfABfABhQBmpBr�Bz�B�B�B�(B�3B�1B�~B�KB�B8,BDwBkbB�B� B�UB�ABδB�B��B�B�B!BLB]BxB~B�B(�B>TBO�BR�BK�BM�BL�BN�BX�BY�B_BjZBozBr�BnxBZ�BG�B��B��BnB��B�(B̧B��B��B��B}�B��B$�B�wB�	BldBb'BH�B'�B	B
�oB
��B
�fB
�0B
�$B
��B
�fB
{�B
_B
M�B
=KB
'�B
-B	�SB	�B	��B	ϽB	�B	��B	gMB	V�B	H�B	@bB	:>B	5B	1B	.�B	%�B	&�B	$�B	B	
B��B��B�B�lB�PB�2B�B��B��BͷBʣBǏBƉBńBŅB�B�sB�jB�kB�bB�BĀB�~B�BŁBŃBłBłBȖBǍB�|B�gB�HB�(B�B�(B�&B�1B�JB�CB�7B�7B�?B�>B�%B�,B�=B�7B�1B�$B�B��B��B��B��B��B��B��B��B�_B�0BnyBP�BAjB>YB9:BAjB5!B:ABBsBK�B?]B@cB=TBE�BI�B\BmpBnwBs�Bw�B��B��B��B�B�!B�$B�;B�HB�SB�RB�QB�SB�QB�^B�~B��B��B��B�}B��B��B��B��B��B��B��B��B��B�'B�B�>B�RBǉB�WB�8B�UB��B��B��BˢBоB��B��B�B�(B�JB�cB�wB�B��B	�B	B	
B	!B	9B	DB	qB	�B	�B	�B	�B	!�B	 �B	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	&�B	*�B	/�B	3B	6B	8%B	=EB	H�B	M�B	P�B	S�B	V�B	X�B	Y�B	Z�B	^B	aB	e1B	iLB	jRB	l\B	mbB	q}B	t�B	v�B	y�B	}�B	��B	��B	��B	�B	�6B	�OB	�ZB	�kB	�~B	�}B	�}B	�B	�B	�}B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�'B	�4B	�CB	�LB	�TB	�gB	�wB	�}B	ǀB	�~B	ǀB	�~B	�vB	�tB	�pB	�qB	�yB	�~B	�~B	ǀB	�yB	ȂB	ʑB	ʏB	̜B	͠B	ϫB	ϭB	гB	дB	еB	гB	ѹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�(B	�.B	�3B	�<B	�=B	�BB	�BB	�AB	�HB	�MB	�YB	�\B	�hB	�mB	�rB	�sB	�xB	�yB	�}B	�B	�|B	�|B	�|B	�}B	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B


B


B
B
B
G�O�B
<B
gB
�B
�B
&�B
+�B
4B
7B
<5B
DgB
K�B
R�B
Y�B
_B
e*B
lWB
poB
s�B
u�B
y�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451152016080714511520160807145115  AO  ARCAADJP                                                                    20150312193442    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150312193442  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150312193442  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145115  IP                  G�O�G�O�G�O�                