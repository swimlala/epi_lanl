CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-09-04T17:02:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170904170205  20190604095307  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�#�ߒ�11   @�#�"�@B@9�A�7K��cU���o1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0��B7��B?��BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy�RD���D�S3D���D��HD�	HD�ND�w
D��
D�qD�A�D�g�D��D�fD�0RD�)D�ҏD��D�.D�|�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @U@�{@�{A
=A'
=AG
=Ag
=A��A��A��A��AÅAӅA�A�BB	BBB!B)B2�\B9\)BA\)BIBQBYBaBiBr(�ByB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC p�Cp�Cp�Cp�Cp�C
p�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:p�C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CVp�CXp�CZ�>C\p�C^p�C`p�Cbp�Cdp�Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5��D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt|)Dy�{D��D�aGD���D��\D�\D�\(D��D��D��D�P D�u�D��(D�zD�>fD�=D��D�(D�<(D��D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A܁A�A�A�  A�ȴA�A�dZA�G�A�/A��A�VA�A���A��mA��/A���A�ĜAپwAٶFA٬AٍPA�K�A�ZA�ƨA՛�AӋDA���A��A�|�A�ƨA�jA��AǾwA�S�A�ĜA���A���A���A�/A�$�A��A�`BA��FA�+A�
=A�v�A�%A�ƨA��A�ffA��TA�=qA���A�v�A��A��A��A���A�E�A��A��PA���A���A��#A��7A�C�A��`A�`BA��uA�%A�K�A�{A��^A�I�A��hA�A�=qA���A�oA���A�1'A��jA�JA���A�$�A�1A��^A�S�A�r�A���A��wA�r�A�z�A�l�A�/A�\)A��HA�A�A�&�A�VA��^A��`A��TA��mA���A�ZA�bA�"�A�(�A�9XA�33A���A��A�5?A�K�A�p�A�
A�A}O�A|  AyhsAw�TAu�As+Aq`BAp�jAo��An{Ak�Aj�yAiƨAh�uAgx�Af�AfQ�Ad9XAb��AaVA`$�A^��A^1A]�A\��AZ(�AXjAV��AU�AT��AS�hASAQ�wAP��AO�#ANbAL��AK��AJȴAJ=qAI�AHA�AGO�AE�wAEAC�
AC
=ABr�AB  AA�AA&�A@A?�A>��A=��A=VA<1'A;�A;&�A:�+A:E�A:�A9S�A7�;A6��A6~�A6A5;dA4bNA3��A3A2VA1�A0VA.�A.1'A-�A,(�A+%A*�A)��A(��A'��A';dA%��A$~�A"�9A"M�A!�TA!�PA!dZA!hsA ��A VA��A�A�A�hA33AA�AG�A�A�#A�A�A|�A��A�mA��AȴA�;AK�Az�A�^A�uAl�A�TA
��A	O�A1'AdZAv�A`BAoA�yA�uA��A�A��A~�AA\)A �DA $�@��^@�j@��@�ff@��7@���@��R@��7@��m@�Q�@�\@�?}@�Q�@��m@�$�@��@��H@�hs@�Q�@��@�Q�@�b@އ+@�X@�+@��@��@�@�O�@�1'@�t�@��@�M�@�$�@���@Ь@Χ�@��`@��
@�33@ʇ+@�`B@�bN@Ǯ@�@��@���@��@��@���@�~�@�hs@��@���@���@�%@��m@�"�@�J@�@��@�&�@��D@���@��@��@��m@���@���@��@�b@���@��@���@��@�  @��@���@��j@�  @�C�@�~�@�G�@�Ĝ@�Z@�  @���@���@�@��7@�G�@���@�(�@�;d@�
=@���@�O�@��/@�j@�1'@�b@�ƨ@�33@��R@�v�@�M�@�-@��7@��9@�Ĝ@��@���@��@�Q�@�z�@�(�@��;@�S�@�"�@���@�ff@�7L@�1@��@�5?@���@��@��@���@���@��D@�9X@��@��P@�;d@���@��T@��#@���@���@��7@�V@���@��9@��@�1'@��m@��w@��@�+@��+@�5?@�{@��@�x�@��@���@���@�b@��w@�t�@�;d@��H@�E�@�@�7L@��/@���@��9@�r�@��@��F@���@���@�l�@�C�@�"�@��y@�v�@�=q@�$�@�J@��#@�x�@�G�@��@���@���@�bN@�9X@�@~�R@}@}?}@}V@|��@|1@{�@{�@{t�@z�@z�!@z�@y�7@xĜ@x�@x �@w|�@u��@u?}@t�D@tz�@tj@t��@s�m@s��@s�@sƨ@s�F@s�@sdZ@s33@r��@r-@rJ@q�#@q��@q��@q��@qG�@p��@p�u@o�;@n�y@nV@nv�@nv�@m�@j�H@d�/@^J�@Y0�@S�[@O�q@Im]@CC�@=@5�o@/+@)�@$w�@��@�@��@T�@=�@	��@@�[111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A܁A�A�A�  A�ȴA�A�dZA�G�A�/A��A�VA�A���A��mA��/A���A�ĜAپwAٶFA٬AٍPA�K�A�ZA�ƨA՛�AӋDA���A��A�|�A�ƨA�jA��AǾwA�S�A�ĜA���A���A���A�/A�$�A��A�`BA��FA�+A�
=A�v�A�%A�ƨA��A�ffA��TA�=qA���A�v�A��A��A��A���A�E�A��A��PA���A���A��#A��7A�C�A��`A�`BA��uA�%A�K�A�{A��^A�I�A��hA�A�=qA���A�oA���A�1'A��jA�JA���A�$�A�1A��^A�S�A�r�A���A��wA�r�A�z�A�l�A�/A�\)A��HA�A�A�&�A�VA��^A��`A��TA��mA���A�ZA�bA�"�A�(�A�9XA�33A���A��A�5?A�K�A�p�A�
A�A}O�A|  AyhsAw�TAu�As+Aq`BAp�jAo��An{Ak�Aj�yAiƨAh�uAgx�Af�AfQ�Ad9XAb��AaVA`$�A^��A^1A]�A\��AZ(�AXjAV��AU�AT��AS�hASAQ�wAP��AO�#ANbAL��AK��AJȴAJ=qAI�AHA�AGO�AE�wAEAC�
AC
=ABr�AB  AA�AA&�A@A?�A>��A=��A=VA<1'A;�A;&�A:�+A:E�A:�A9S�A7�;A6��A6~�A6A5;dA4bNA3��A3A2VA1�A0VA.�A.1'A-�A,(�A+%A*�A)��A(��A'��A';dA%��A$~�A"�9A"M�A!�TA!�PA!dZA!hsA ��A VA��A�A�A�hA33AA�AG�A�A�#A�A�A|�A��A�mA��AȴA�;AK�Az�A�^A�uAl�A�TA
��A	O�A1'AdZAv�A`BAoA�yA�uA��A�A��A~�AA\)A �DA $�@��^@�j@��@�ff@��7@���@��R@��7@��m@�Q�@�\@�?}@�Q�@��m@�$�@��@��H@�hs@�Q�@��@�Q�@�b@އ+@�X@�+@��@��@�@�O�@�1'@�t�@��@�M�@�$�@���@Ь@Χ�@��`@��
@�33@ʇ+@�`B@�bN@Ǯ@�@��@���@��@��@���@�~�@�hs@��@���@���@�%@��m@�"�@�J@�@��@�&�@��D@���@��@��@��m@���@���@��@�b@���@��@���@��@�  @��@���@��j@�  @�C�@�~�@�G�@�Ĝ@�Z@�  @���@���@�@��7@�G�@���@�(�@�;d@�
=@���@�O�@��/@�j@�1'@�b@�ƨ@�33@��R@�v�@�M�@�-@��7@��9@�Ĝ@��@���@��@�Q�@�z�@�(�@��;@�S�@�"�@���@�ff@�7L@�1@��@�5?@���@��@��@���@���@��D@�9X@��@��P@�;d@���@��T@��#@���@���@��7@�V@���@��9@��@�1'@��m@��w@��@�+@��+@�5?@�{@��@�x�@��@���@���@�b@��w@�t�@�;d@��H@�E�@�@�7L@��/@���@��9@�r�@��@��F@���@���@�l�@�C�@�"�@��y@�v�@�=q@�$�@�J@��#@�x�@�G�@��@���@���@�bN@�9X@�@~�R@}@}?}@}V@|��@|1@{�@{�@{t�@z�@z�!@z�@y�7@xĜ@x�@x �@w|�@u��@u?}@t�D@tz�@tj@t��@s�m@s��@s�@sƨ@s�F@s�@sdZ@s33@r��@r-@rJ@q�#@q��@q��@q��@qG�@p��@p�u@o�;@n�y@nV@nv�@nv�G�O�@j�H@d�/@^J�@Y0�@S�[@O�q@Im]@CC�@=@5�o@/+@)�@$w�@��@�@��@T�@=�@	��@@�[111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTB_;BZBXBQ�BO�BN�BN�BN�BM�BM�BL�BJ�BJ�BI�BH�BH�BH�BG�BF�BP�Bs�B�DB��B�qB{B��BT�B�hBS�B\B�/B�B�TB�NB�B��BB\B�B�B �B%�B1'B49B49B:^BF�BO�BVB\)B_;BbNBcTBe`BgmBk�Bk�BiyBp�Bm�B^5BS�BQ�BO�BL�BG�B?}B+B�B�B{BPB  B�B�B�mB�TB �B33B$�B�BDB��B�B�TB��BȴB�}B�FB�!B��B��B�=B�By�Bp�B_;BT�BL�BE�BI�BL�BQ�BK�B,BB
�B
�B
�ZB
��B
B
�-B
��B
�bB
�+B
~�B
r�B
ffB
R�B
F�B
6FB
�B
oB
\B
%B	��B	�yB	�ZB	�ZB	�#B	��B	��B	��B	�dB	�!B	��B	��B	��B	��B	�hB	�DB	r�B	cTB	YB	R�B	K�B	F�B	A�B	9XB	33B	,B	#�B	�B	�B	hB	PB		7B	B��B��B��B�B�B�B�B�sB�fB�NB�5B�#B�
B��B��B��B��B��B��B��B��BǮBĜBB��B�qB�^B�LB�9B�-B�B��B��B��B��B�{B�uB�uB�VB�=B�+B�B~�Bx�Br�Bp�Bp�Bq�Bq�Bu�Bu�Bt�Br�Bo�Bn�Bm�BjBiyBffBcTBaHB_;B]/B[#BXBT�BP�BL�BJ�BG�BE�BD�BB�B>wB8RB2-B.B)�B(�B%�B$�B$�B#�B"�B"�B#�B(�B(�B'�B(�B'�B%�B$�B#�B#�B"�B �B�B�B�B�B�B{B{B{BoBuBuBoBhB\BDB1B+B	7B1B	7B	7B1B%BBBB  B  B��B��B��B��B��B��B��B��BBBBBB  B
=B	7B+BBBB+B1B	7BJBPBhBoB�B�B�B�B�B�B"�B-B49B6FB7LB7LB7LB9XB:^B?}BC�BG�BK�BM�BO�BP�BT�BW
BXBZBZBZB\)B]/B^5B_;BbNBe`BffBhsBn�Bp�Br�Bu�B{�B� B�B�B�B�7B�JB�VB�PB�\B�oB��B��B��B��B�B�B�3B�9B�?B�?B�FB�?B�3B�-B�-B�FB�LB�RB�XB�^B�^B�^B�jB�wB��BƨBȴBɺB��B��B��B��B��B�
B�B�5B�BB�NB�ZB�sB�yB�yB�yB�B�B�B�B��B��B��B��B	B	%B	+B	1B		7B	JB	JB	PB	\B	oB	�B	�B	�B	�B	�B	�B	%�B	'�B	(�B	)�B	+B	0!B	2-B	49B	7LB	:^B	=qB	?}B	B�B	H�B	M�B	O�B	P�B	Q�B	Q�B	S�B	YB	YB	\)B	]/B	`BB	cTB	dZB	dZB	ffB	hsB	hsB	iyB	k�B	n�B	o�B	q�B	r�B	s�B	t�B	u�B	v�B	w�B	x�B	x�B	y�B	{�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�DB	��B	��B	֡B	�kB
�B
JB
_B
&2B
2aB
:DB
CGB
J=B
P�B
X�B
_�B
d�B
j�B
o�B
u%B
x8B
{111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bc2B_BY�BW�BQ�BO�BN�BN�BN�BM�BM�BL�BJ�BJ�BI�BH�BH�BH�BG�BF�BP�Bs�B�#B��B�KG�O�B��BT�B�FBS�B7B�B��B�1B�'B�B��B �B4BcBtB �B%�B1B4B4B:7BF�BO�BU�B\B_Bb$Bc/Be5BgEBk^Bk]BiSBp�BmiB^BS�BQ�BO�BL�BG�B?RB*�B�BxBSB%B��B�B�^B�FB�)B �B3B$�ByBB��B�B�*B��BȌB�TB�B��B��B�VB�B��By�Bp{B_BT�BL�BEyBI�BL�BQ�BK�B+�B �B
�B
�qB
�,B
ѿB
�dB
�B
��B
�6B
��B
~�B
r�B
f;B
R�B
FzB
6B
�B
@B
.B
�B	��B	�IB	�,B	�,B	��B	��B	��B	жB	�6B	��B	��B	��B	��B	�^B	�8B	�B	r�B	c&B	X�B	R�B	K�B	FwB	A[B	9(B	3B	+�B	#�B	vB	OB	9B	B		B	�B��B��B��B�tB�\B�SB�PB�BB�4B�B�B��B��B��B��BгBϭB̛B˕B͠B̛B�{B�jB�bB�QB�@B�,B�B�B��B��B��B��B�zB�\B�IB�CB�@B�%B�B��B��B~�Bx�Br}BpqBpqBqvBqxBu�Bu�Bt�BrBojBncBm^BjKBiHBf5Bc!BaB_B\�BZ�BW�BT�BP�BL�BJ�BGzBEpBDgBB[B>CB8B1�B-�B)�B(�B%�B$�B$�B#�B"�B"�B#�B(�B(�B'�B(�B'�B%�B$�B#�B#�B"�B �B�B}BrB]BNBEBFBCB8B?B@B;B1B%B
B�B�B	 B�B�B�B�B�B�B �B �B��B��B��B��B��B��B��B��B��B��B�B�B �B�B �B��B
B�B�B�B�B�B�B�B�BBB2B6BIBjBzBbBTBMB"�B,�B4 B6B7B7B7B9 B:%B?BBC]BGtBK�BM�BO�BP�BT�BV�BW�BY�BY�BY�B[�B\�B]�B_BbBe(Bf.Bh<Bn_BpjBruBu�B{�B�B��B��B��B��B�B� B�B�$B�5B�IB�mB�rB��B��B��B��B� B�B�B�B�B��B��B��B�B�B�B�B�$B�%B�#B�/B�@B�FB�mB�}BɀBʉB̑BѳBӿB��B��B��B��B�	B�B�!B�8B�BB�@B�>B�SB�\B�dB�xB��B��B��B��B	 �B	�B	�B	�B		 B	B	B	B	!B	7B	OB	RB	VB	gB	oB	�B	%�B	'�B	(�B	)�B	*�B	/�B	1�B	3�B	7B	:"B	=6B	?CB	BUB	HuB	M�B	O�B	P�B	Q�B	Q�B	S�B	X�B	X�B	[�B	\�B	`B	cB	d B	d!B	f)B	h7B	h9B	i>B	kKB	n^B	ofB	qqB	rtB	s{B	t�B	u�B	v�B	w�B	x�B	x�B	y�B	{�B	{�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	��B	�eB	�1B
FB
B
#B
%�B
2'B
:	B
CB
JB
P�B
X�B
_�B
d�B
jxB
ofB
t�B
w�B
z�111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953072019060409530720190604095307  AO  ARCAADJP                                                                    20170904170205    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170904170205  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170904170205  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095307  IP                  G�O�G�O�G�O�                