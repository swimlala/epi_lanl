CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:21Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170921  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               vA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @����x1   @��F)�b@5��/���c'�vȴ91   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    vA   B   B   @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�3D�c�D��qD��D�#�D�MD���D��)D��D�XRD���D��RD�"�D�T�Dڣ3D��3D�D�VfD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�z�@�z�A=qA>=qA_�A~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�aHB�ǮB�ǮB��{B�ǮB��{B�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3�qC5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck�qCm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dr�D�Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCr�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DP\DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt�)Dy��D��D�`RD���D��)D� RD�I�D��D�ؤD�D�T�D��>D���D�
D�QHDڟ�D�ϮD�
�D�R�D�3D��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��A��A��#A��#A��#A��/A��/A��/A��/A��/A��/A��;A��HA��TA��TA��TA��mA��A��A��A��yA��yA��A��A��A��yA��A��A��yA��A��A��A��A��A��A��A��A��`A��A��yA���A��yA���A�~�A�1'A���A�~�A�C�A���A��mA�oA��RA�ȴA� �A�z�A��A�dZA�x�A��-A�K�A��A�XA�x�A�ZA�;dA�`BA�1'A��#A��`A�r�A��HA��;A��TA�;dA���A�-A���A���A��jA�+A��A��DA��jA���A��A�
=A�33A��7A��A�O�A�bNA��PA��uA��A��A���A�33A��!A���A��wA��A��#A�l�A�C�A���A�{A�`BA��`A�n�A��A�A|��A|JAy%Au|�ApVAlĜAh{Ae�Ad��AcA`JA^�jA\Q�AY��AW&�AT�jAR�yAQ��AO�hAM�AMp�AMO�ALE�AJ9XAH�AHz�AG�7AE/AC?}AA�
A?�^A>�A<��A;�7A9��A8 �A6�jA5l�A4JA2�DA1�PA1;dA1
=A0�RA/�
A.�+A,�uA+�A*��A*{A)��A)VA(�jA'S�A&��A&JA%&�A$��A"�HA"�jA"v�A"  A!�PA ��AVA=qAx�A$�A��A?}A��A&�A�A�#A��A�/A{A��Al�AĜAVAz�AbAE�A�An�AjA��AZA��A-AXA�jA=qA�wA
9XA	�A	��A~�AJA��A�
A��A��AVAA��AA7LA ��@�ff@�+@�33@�dZ@��@��@���@�J@�I�@��@���@�7@��@�-@�n�@�@�dZ@�@��T@�p�@��@���@�Q�@�"�@�`B@��@߶F@��@�-@���@��@�Z@���@�|�@�l�@�n�@ղ-@�?}@�A�@�S�@���@�O�@�C�@��@��@́@��@�"�@�+@��H@���@�C�@�n�@�ȴ@��@�ƨ@�x�@��@�l�@���@���@���@�Z@��;@�
=@��!@�=q@�hs@�ff@���@�ƨ@��D@� �@�|�@�n�@�X@��D@�(�@�E�@��T@��@���@��T@��@��@���@��T@�{@�n�@�5?@�V@�"�@���@�5?@�5?@�ff@��@�V@�E�@��7@���@�ƨ@��@�V@�{@��-@�J@�ff@�ȴ@�S�@�o@�"�@�
=@�~�@��@�%@�z�@�l�@�=q@�@�=q@�^5@�V@�I�@��!@�7L@�1@�dZ@�C�@�=q@��@�j@��@��F@�|�@�dZ@�C�@�;d@��@�K�@�;d@�
=@�"�@�dZ@�33@���@���@��\@�~�@���@��j@���@��D@��D@��D@�r�@�bN@���@��w@���@�C�@�t�@�|�@�t�@�l�@�S�@�+@�@��@�ȴ@���@�v�@��^@��@�hs@�G�@�/@�V@��j@�r�@�I�@��@��@��H@�~�@�=q@�{@��T@���@�7L@��`@���@��9@���@��u@�z�@�Q�@�A�@�(�@���@��;@��w@���@�l�@�"�@��@�
=@��y@���@�v�@�M�@�{@���@��#@�@��^@���@���@�V@��9@��@��D@�r�@�r�@�r�@�j@�bN@�Q�@� �@�b@��m@��w@���@��@�\)@��@���@���@�n�@�^5@�=q@�@���@���@���@��7@�p�@�G�@��@��`@���@�A�@�ƨ@�t�@�;d@�"�@�o@���@���@�n�@��@��#@��h@�7L@���@��j@��@}�^@t�@nW�@d�p@]�M@X��@R_�@K��@E�~@>W�@6v�@1e,@-j@'�4@ 1'@��@�@��@��@P�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��
A��A��A��#A��#A��#A��/A��/A��/A��/A��/A��/A��;A��HA��TA��TA��TA��mA��A��A��A��yA��yA��A��A��A��yA��A��A��yA��A��A��A��A��A��A��A��A��`A��A��yA���A��yA���A�~�A�1'A���A�~�A�C�A���A��mA�oA��RA�ȴA� �A�z�A��A�dZA�x�A��-A�K�A��A�XA�x�A�ZA�;dA�`BA�1'A��#A��`A�r�A��HA��;A��TA�;dA���A�-A���A���A��jA�+A��A��DA��jA���A��A�
=A�33A��7A��A�O�A�bNA��PA��uA��A��A���A�33A��!A���A��wA��A��#A�l�A�C�A���A�{A�`BA��`A�n�A��A�A|��A|JAy%Au|�ApVAlĜAh{Ae�Ad��AcA`JA^�jA\Q�AY��AW&�AT�jAR�yAQ��AO�hAM�AMp�AMO�ALE�AJ9XAH�AHz�AG�7AE/AC?}AA�
A?�^A>�A<��A;�7A9��A8 �A6�jA5l�A4JA2�DA1�PA1;dA1
=A0�RA/�
A.�+A,�uA+�A*��A*{A)��A)VA(�jA'S�A&��A&JA%&�A$��A"�HA"�jA"v�A"  A!�PA ��AVA=qAx�A$�A��A?}A��A&�A�A�#A��A�/A{A��Al�AĜAVAz�AbAE�A�An�AjA��AZA��A-AXA�jA=qA�wA
9XA	�A	��A~�AJA��A�
A��A��AVAA��AA7LA ��@�ff@�+@�33@�dZ@��@��@���@�J@�I�@��@���@�7@��@�-@�n�@�@�dZ@�@��T@�p�@��@���@�Q�@�"�@�`B@��@߶F@��@�-@���@��@�Z@���@�|�@�l�@�n�@ղ-@�?}@�A�@�S�@���@�O�@�C�@��@��@́@��@�"�@�+@��H@���@�C�@�n�@�ȴ@��@�ƨ@�x�@��@�l�@���@���@���@�Z@��;@�
=@��!@�=q@�hs@�ff@���@�ƨ@��D@� �@�|�@�n�@�X@��D@�(�@�E�@��T@��@���@��T@��@��@���@��T@�{@�n�@�5?@�V@�"�@���@�5?@�5?@�ff@��@�V@�E�@��7@���@�ƨ@��@�V@�{@��-@�J@�ff@�ȴ@�S�@�o@�"�@�
=@�~�@��@�%@�z�@�l�@�=q@�@�=q@�^5@�V@�I�@��!@�7L@�1@�dZ@�C�@�=q@��@�j@��@��F@�|�@�dZ@�C�@�;d@��@�K�@�;d@�
=@�"�@�dZ@�33@���@���@��\@�~�@���@��j@���@��D@��D@��D@�r�@�bN@���@��w@���@�C�@�t�@�|�@�t�@�l�@�S�@�+@�@��@�ȴ@���@�v�@��^@��@�hs@�G�@�/@�V@��j@�r�@�I�@��@��@��H@�~�@�=q@�{@��T@���@�7L@��`@���@��9@���@��u@�z�@�Q�@�A�@�(�@���@��;@��w@���@�l�@�"�@��@�
=@��y@���@�v�@�M�@�{@���@��#@�@��^@���@���@�V@��9@��@��D@�r�@�r�@�r�@�j@�bN@�Q�@� �@�b@��m@��w@���@��@�\)@��@���@���@�n�@�^5@�=q@�@���@���@���@��7@�p�@�G�@��@��`@���@�A�@�ƨ@�t�@�;d@�"�@�o@���@���@�n�@��@��#@��h@�7L@���@��jG�O�@}�^@t�@nW�@d�p@]�M@X��@R_�@K��@E�~@>W�@6v�@1e,@-j@'�4@ 1'@��@�@��@��@P�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B/BZBdZBz�B�PB��B�B�!B��B�3B�NB��B��B
=B{B�B!�B$�B%�B$�B'�B�B{BDB+B��B��B�B�B�mB�HB�#B��B��BɺB��BĜB�}B��B�B��BĜBǮB�B�/B�BÖB�^BŢBÖB�B�bB|�Bu�Bm�BffB^5BR�B.B�BB
�B
��B
ǮB
�!B
�VB
�B
}�B
iyB
A�B
-B
"�B

=B	�B	ƨB	�B	�DB	~�B	s�B	k�B	YB	O�B	D�B	9XB	,B	�B	DB	B��B��B	+B	
=B	\B	+B	  B��B��B��B�B�ZB�#B��BǮB�jB�RB�!B��B��B��B��B�{B�oB�hB�bB�PB�JB�1B�B�B}�B|�B{�By�By�Bu�Bs�Bo�Bo�Bl�Bm�Bo�Bp�Bn�Bo�Bk�Be`BcTBZBT�BR�BP�BN�BL�BK�BK�BL�BM�BN�BM�BXBYB[#BZBQ�BL�BM�BS�BW
BjBu�Bx�Bv�Bv�Bt�Bn�BgmBffBs�Bt�By�Bx�Bv�Bt�Bw�By�Bx�Bw�Bv�Bu�Bq�BdZB[#BH�BK�BF�B:^B6FB5?B8RB:^B@�BH�BJ�BXBaHB]/B]/BcTBcTBcTBdZBe`BgmBiyBjBiyBhsBhsBhsBk�Bl�Bk�Bk�Bk�Bl�Bq�Br�Bs�Bu�Bu�Bx�Bx�Bx�By�Bx�By�Bz�B}�Br�B{�By�Bs�Bx�Bx�Bu�Bl�BffBiyBjBk�Bk�Bm�Bo�Br�Bu�Bu�Bx�B}�B�7B��B��B��B�!B�-B�3B�?B�?B�FB�RB�wBƨBɺB��B��B�B�B�B�#B�BB�HB�HB�;B�BB�ZB�`B�yB�B��B��B��B��B��B��B��B��B��B	  B	B	%B	VB	\B	hB	oB	bB	hB	\B	PB	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	+B	2-B	6FB	7LB	;dB	?}B	@�B	B�B	C�B	F�B	J�B	L�B	M�B	O�B	P�B	Q�B	T�B	W
B	\)B	`BB	cTB	e`B	hsB	jB	k�B	m�B	o�B	q�B	r�B	t�B	x�B	y�B	y�B	|�B	|�B	}�B	~�B	� B	�B	�B	�%B	�+B	�1B	�7B	�7B	�DB	�PB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�FB	�RB	�XB	�^B	�dB	�dB	�dB	�}B	B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�fB	�mB	�sB	�yB	�yB	�yB	�B	��B

	B
gB
�B
+�B
4B
;�B
@B
E�B
L�B
Q�B
UMB
ZQB
`BB
d�B
iB
l=B
p�B
tB
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�\B
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�VB
�\B
�\B
�bB
�\B
�\B
�\B
�\B
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�VB#�BN�BX�BoXB��B�?B��B��B�pB��B־B�7B�8B��B�BB7BIBOBIB\BB�B��B��B�eB�5B�B�B��BսBϘB�tB�VB�2B�JB�B��B�KB̇B�uB�B�'BΔBѦB͏B�B��B�B�B��B��BqpBjFBbBZ�BR�BGxB"�B&B
��B
�B
ɑB
�BB
��B
��B
x�B
r�B
^B
6+B
!�B
vB	��B	�5B	�VB	��B	�B	s�B	hmB	`=B	M�B	D�B	9XB	.B	 �B	aB	 B��B�B�B��B� B	B��B��B�B�B�B�LB�"B��BƶB�zB�7B�B��B��B��B��B�kB�MB�AB�:B�4B�"B�B}Bw�Bu�Br�Bq�Bp�Bn�Bn�Bj�Bh�BduBduBabBbhBduBe{BcoBduB`]BZ9BX-BN�BI�BG�BE�BC�BA�B@�B@�BA�BB�BC�BB�BL�BM�BO�BN�BF�BA�BB�BH�BK�B_ZBj�Bm�Bk�Bk�Bi�BcsB\IB[BBh�Bi�Bn�Bm�Bk�Bi�Bl�Bn�Bm�Bl�Bk�Bj�Bf�BY8BPB=�B@�B;�B/@B+)B*"B-5B/AB5fB=�B?�BL�BV(BRBRBX4BX4BX4BY:BZ@B\MB^YB__B^YB]SB]SB]SB`eBakB`eB`eB`fBakBf�Bg�Bh�Bj�Bj�Bm�Bm�Bm�Bn�Bm�Bn�Bo�Br�Bg�Bp�Bn�Bh�Bm�Bm�Bj�BanB[JB^]B_bB`hB`iBbtBd�Bg�Bj�Bj�Bm�Br�B~B��B��B��B��B�B�B�B�B�$B�0B�TB��B��B��B��B��B��B��B��B�B�"B�"B�B�B�4B�:B�SB�B�B�B�B�B��B��B��B��B��B��B��B��B	-B	3B	?B	FB	9B	?B	3B	'B	3B	FB	^B	pB	�B	�B	�B	wB	�B	�B	�B	�B	�B	wB	eB	_B	kB	qB	�B	�B	�B	�B	�B	'B	+B	,!B	09B	4QB	5WB	7cB	8jB	;|B	?�B	A�B	B�B	D�B	E�B	F�B	I�B	K�B	P�B	UB	X&B	Z1B	]DB	_PB	`VB	bbB	doB	fzB	g�B	i�B	m�B	n�B	n�B	q�B	q�B	r�B	s�B	t�B	u�B	w�B	z�B	{�B	} B	~B	~B	�B	�B	�0B	�6B	�CB	�[B	�hB	�mB	�mB	�sB	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�/B	�/B	�HB	�YB	�YB	�`B	�fB	�fB	�fB	�lB	�lB	�lB	�rB	�rB	�xB	�~B	��B	��B	��B	B	ĨB	ŮB	ƵB	ƵB	ǻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�"B	�.B	�5B	�;B	�AB	�AG�O�B	��B	�B	��B

,B
�B
 �B
(�B
0�B
4�B
:eB
A[B
F�B
JB
OB
UB
YkB
]�B
`�B
eeB
h�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170921    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170921  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170921  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                