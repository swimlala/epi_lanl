CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-02-22T02:15:10Z creation; 2023-04-26T19:14:25Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Y�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  `�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    }(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   !   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � ((   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   D�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � K�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` hH   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   h�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   n�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   t�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T z�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   z�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   {   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   {   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   {   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � {   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   {�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   {�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    {�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        {�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        {�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       {�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    {�Argo profile    3.1 1.2 19500101000000  20180222021510  20230426191425  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_003                 7316_008644_003                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�NCҝ�&@�NCҝ�&11  @�ND    @�ND    @*%;:@*%;:�d4S����d4S���11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@=p�@}p�@�  @�G�@�\A   A  A!G�A-p�A@��A_\)A\)A�Q�A�Q�A���A�  A�  A�Q�A�Q�B (�B�
B�
B(�B   B(  B0  B8  B?�
BH  BPQ�BX  B_�
Bg�
Bo�
Bx(�B�{B�  B�  B��B��B�  B�{B�{B�{B�  B�  B�{B�  B��B��
B��B��B�  B�{B��B��B�{B�{B�  B��B�  B�(�B�  B��
B�  B�{B�  C (�C��C��C��C  C	��C  C  C��C  C  C
=C
=C  C
=C
=C 
=C"  C$  C&  C(  C*  C,  C.  C0  C1��C3��C5��C8  C9��C;��C>  C@
=CB  CC�CE��CH  CJ  CL
=CM��CO��CR  CT  CU��CX  CZ  C\  C^  C`  Cb
=Cd  Ce��Ch  Cj  Cl  Cm��Co��Cq��Cs��Cv  Cx
=Cz
=C|
=C~  C�  C�  C���C�  C�
=C�C�  C�C�  C�C�C�  C���C�  C�
=C�C���C�  C�C�  C���C���C�C�
=C�  C�  C�  C�  C�C�  C���C���C�  C�  C�  C�C�C�C�  C�  C�  C���C���C���C���C�  C���C���C�  C�C�  C�  C�C�C�  C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C�  C���C�  C���C�  C�
=C�C�  C�  C�C���C�  C�  C���C���C�  C�  C���C�  C�  C���C�  C�C�C�C�C�  C�  C�  C�  C�  C���C�C�  C�C�C���C�  C�C�C�  C�  C�  C�C�C�C�  C���C�  C�  C�  C���C���C�C�
=C�
=C�
=C�  C���C���C�  C�
=D �D ��D�D��D  D��D�D��D�D� D�qD� D�qDz�D�qD��DD��D	  D	� D
  D
}qD  D��D  D� D�qD� D  D� D  D��D�D� D�qD}qD  D� D�D��D  D}qD�qD� D�qD� D  D� D�D��D�D� D�D� D�qD}qD�qD� D  D� D�D� D�qD� D�qD � D!  D!}qD"�D"� D"�qD#� D$  D$}qD%  D%}qD%�qD&� D'  D'� D(  D(� D)�D)� D*  D*� D+  D+� D,  D,� D-  D-� D-�qD.}qD/  D/� D0  D0��D1  D1��D2  D2� D3�D3� D3�qD4� D5  D5� D5�qD6}qD6�qD7� D8�D8}qD9  D9��D9�qD:}qD;  D;��D<�D<��D=D=� D=�qD>� D>�qD?� D@�D@� D@�qDA� DB  DB��DC�DC�DD�DD� DE�DE��DE�qDF� DG  DG}qDG�qDH}qDH�qDI� DJ  DJ��DK  DK}qDL  DL��DM�DM� DN  DN� DN�qDO}qDO��DP� DQ�DQ��DR�DR� DS  DS� DT�DT� DU  DU}qDV  DV��DW  DW��DX�DX� DY  DY��DY�qDZ� D[  D[��D\�D\� D\�qD]� D^  D^��D_�D_��D`  D`��DaDa� Da�qDb}qDb�qDc}qDd�Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di�qDj}qDk  Dk� Dl  Dl}qDl�qDm}qDm�qDnz�Dn��Doz�Do�qDp� Dq  Dq}qDr  Dr� Ds  Ds� Dt  Dt��Du  Du� Dv�Dv}qDw  Dw� Dx�Dx�Dy�Dy� Dz  Dz� Dz�qD{� D|�D|� D}  D}��D}�qD~� D~�qDz�D�  D�AHD��HD�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D��HD�D��D�B�D�~�D��qD���D�>�D�� D���D���D�@ D�� D���D�  D�@ D�~�D��HD�HD�AHD�� D���D�HD�@ D�� D���D�  D�@ D�� D�� D�  D�AHD�~�D�� D�  D�>�D�� D�D�HD�>�D�~�D��qD�  D�AHD��HD�� D�  D�>�D�}qD��qD��qD�@ D�� D���D�HD�AHD�� D�� D���D�>�D�� D�� D�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�=qD�}qD��qD��qD�=qD�}qD�� D�  D�=qD�~�D���D�  D�AHD�~�D�� D�  D�@ D��HD�� D�  D�AHD�~�D�� D�HD�@ D�� D���D���D�>�D�~�D�� D�  D�>�D�~�D��HD�HD�>�D�� D��HD�HD�AHD�~�D���D�  D�@ D�� D�� D�HD�B�D��HD��HD�HD�AHD��HD��HD�  D�@ D�~�D�� D�HD�AHD��HD��HD�  D�@ D��HD��HD���D�@ D�� D���D���D�>�D�~�D�� D�  D�>�D�� D�� D�  D�AHD�~�D���D�  D�AHD�� D���D�  D�@ D�� D��HD�HD�AHD���D�� D���D�@ D��HD�� D�  D�>�D�~�D���D�  D�@ D��HD�� D�  D�@ D�}qD���D�  D�AHD�~�D�� D�HD�AHD��HD�� D�  D�@ D�� D��HD���D�@ D�� D���D���D�>�D��HD�� D�  D�@ D��HD��HD�  D�AHD���D���D���D�AHD��HD��HD�HD�AHD��HD�D��D�B�D���D�D�HD�@ D�� D��HD�HD�>�D��HD�� D�  D�>�D�� D�D��D�,�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?��?aG�?�\)?�33?�
=?�@\)@!G�@.{@@  @Q�@c�
@u@��\@���@�
=@�G�@���@���@�(�@Ǯ@�\)@ٙ�@��@�\)@�Q�A ��AA�A  Az�A��A\)A$z�A'�A.{A333A7�A<(�AAG�AG
=AL(�AQG�AU�AZ=qA`  Adz�AhQ�Amp�Ar�\Aw�A|(�A�  A��\A�p�A�\)A���A��
A��RA���A��HA��A�  A�=qA�(�A�ffA�G�A��A�p�A�  A��\A�z�A�
=A���A�33A�A�  A��A�z�A�
=A���AÅA�A�Q�Aʏ\A�p�A�\)Aљ�A�(�A�
=A�G�A�33A�p�A�Q�A�33A��A�A�=qA��A�  A�=qA���A��A��\A��A�\)B ��BffB�
B�B=qB\)B��B
=qB\)B��BB
=Bz�Bp�BffB33Bz�Bp�B=qB�HB�Bz�B�BB�\B\)B  B��B�B{B�HB\)B   B ��B!p�B"ffB#33B#�B$(�B%�B%�B&�\B'33B(  B(��B)p�B)�B*�RB+�B,Q�B,��B-��B.=qB/
=B0  B0��B1�B1�B2�RB3�B4(�B4��B5��B6�\B7\)B7�
B8��B9p�B:ffB;33B;�
B<z�B=G�B>{B?
=B?�B@Q�BA�BB{BB�HBC�BD(�BE�BF{BF�RBG\)BH  BH��BI�BJ�RBK\)BL  BL��BM�BN�RBO33BO�
BP��BQBRffBS
=BT  BT��BUBVffBW33BX  BX��BY��BZffB[33B\  B\��B]�B^�\B_\)B`Q�BaG�Bb{Bb�RBc�Bd��Be��Bf=qBg
=Bg�
Bh��BiBj�\Bk\)BlQ�Bmp�Bn=qBn�HBo�Bp��BqBr�RBs\)Bt(�Bu�Bv{Bw33Bw�
Bx��By��Bz�RB{�B|��B}p�B~=qB
=B�{B��\B�
=B�p�B��
B�=qB��RB�G�B�B�(�B��\B���B�p�B��B�z�B��HB�\)B�B�(�B��RB�33B���B�{B�z�B���B��B�  B�z�B��HB�G�B��B�=qB��HB�G�B��B�(�B���B�33B��B�{B��\B��B���B�(�B��RB��B���B�{B��RB�G�B��
B�ffB���B�p�B�  B�z�B�
=B��B�=qB��HB�p�B��B�ffB���B���B�=qB��HB�\)B��B��\B�
=B��
B�ffB���B��B�  B���B�G�B��
B�z�B��B�B�Q�B��HB�p�B�  B��\B�G�B��B��\B��B��B�=qB���B�p�B�{B��RB�\)B��B��\B��B��B�Q�B��HB��B�(�B��RB�\)B�  B��RB�\)B�  B��\B��B��B�ffB�
=B��B�Q�B��HB�p�B�  B\B�33B�B�z�B�
=BŮB�Q�B���BǅB�(�BȸRB�33B�B�Q�B���B˙�B�=qB���B�p�B�{BΏ\B��BϮB�Q�B��HBхB�(�B���B�p�B�  Bԣ�B��B�B�Q�B��HB�p�B�  B؏\B�33B�B�Q�B���BۅB�(�BܸRB�G�B��Bޏ\B��B߮B�Q�B���B�B�{B��B�33B�B�Q�B��HB�\)B�  B�\B��B癚B�=qB���B�\)B��B�z�B��B�B�=qB��HB�p�B�{B��B�33B�B�ffB��HB�B�  B��B�33B�B�Q�B���B�\)B�  B�z�B�
=B���B�(�B��RB�G�B��
B�ffB���B��B�{B���B��B��B�(�B���B��B��B��
C �C G�C p�C ��C �RC �
C ��C{C(�CG�C\)Cp�C�\C��C�RC��C�C{C(�CG�Cp�C�C��CC�HC��C{C33CQ�CffCz�C��CC�HC  C�C=qCffCz�C��CC�C
=C33CQ�Cp�C��CC�C
=C33C\)Cz�C��CC�C{C=qC\)C�C�C��C��C�CG�Cp�C�\C�RC�HC	
=C	33C	Q�C	z�C	�C	��C	��C
�C
G�C
p�C
��C
C
��C�CG�Cp�C��CC�C�CG�Cp�C��CC�C�CG�Cp�C��CC�C{C=qCffC��CC�C
=C33CffC�\C�RC�HC
=C33C\)C�C�RC�HC
=C33C\)C�C�RC�HC
=C=qCffC�\C�RC�HC
=C33CQ�Cz�C�C�
C
=C33C\)C�C��C�
C  C(�CQ�C�C�C�
C  C(�CQ�Cz�C�C��C��C�CG�Cp�C��CC�HC{C33C\)C�C�C�
C  C�CG�CffC�\CC�HC{C=qC\)C�C�C�
C  C(�CQ�C�C�C�
C  C(�CQ�Cz�C�C��C��C�CG�Cp�C��C�RC�HC
=C33C\)C�C�C��C��C�CG�Cp�C��CC�C 
=C 33C \)C z�C �C ��C ��C!�C!G�C!p�C!��C!C!�C"{C"=qC"ffC"�\C"�C"�HC#  C#33C#Q�C#z�C#��C#��C#�C$�C$=qC$ffC$�C$�C$�
C%  C%(�C%Q�C%z�C%�C%�
C&  C&(�C&Q�C&z�C&��C&C&�C'{C'=qC'\)C'�C'�RC'�HC(
=C(=qC(ffC(��C(C(�C){C)=qC)ffC)�\C)�C)�
C*
=C*33C*\)C*�\C*C*��C+�C+G�C+p�C+��C+��C+��C,�C,G�C,z�C,��C,�
C-
=C-=qC-ffC-��C-C-�C.�C.=qC.p�C.��C.C.��C/�C/Q�C/�C/�C/�HC0{C0=qC0p�C0�\C0C0�C1{C1=qC1p�C1��C1��C1��C2(�C2\)C2��C2��C2��C3(�C3\)C3�\C3�RC3�C4�C4Q�C4z�C4�C4�
C5
=C5=qC5p�C5��C5�HC6{C6G�C6z�C6�C6�HC7{C7G�C7z�C7��C7��C8  C833C8ffC8��C8��C9
=C9=qC9p�C9�C9�
C:
=C:=qC:ffC:��C:��C;
=C;=qC;z�C;�C;�C<�C<Q�C<�C<�RC<�C=�C=Q�C=z�C=�RC=�C>(�C>p�C>��C>�HC?(�C?\)C?��C?�
C@
=C@=qC@z�C@�RC@��CA=qCAz�CA�RCA��CB(�CBffCB��CB�
CC{CCQ�CC��CC�
CD{CDQ�CD�\CDCE  CE33CEp�CE�CE��CF33CFz�CF�RCF�CG(�CG\)CG��CG�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  ?��H@=p�@}p�@�  @�G�@�\A   A  A!G�A-p�A@��A_\)A\)A�Q�A�Q�A���A�  A�  A�Q�A�Q�B (�B�
B�
B(�B   B(  B0  B8  B?�
BH  BPQ�BX  B_�
Bg�
Bo�
Bx(�B�{B�  B�  B��B��B�  B�{B�{B�{B�  B�  B�{B�  B��B��
B��B��B�  B�{B��B��B�{B�{B�  B��B�  B�(�B�  B��
B�  B�{B�  C (�C��C��C��C  C	��C  C  C��C  C  C
=C
=C  C
=C
=C 
=C"  C$  C&  C(  C*  C,  C.  C0  C1��C3��C5��C8  C9��C;��C>  C@
=CB  CC�CE��CH  CJ  CL
=CM��CO��CR  CT  CU��CX  CZ  C\  C^  C`  Cb
=Cd  Ce��Ch  Cj  Cl  Cm��Co��Cq��Cs��Cv  Cx
=Cz
=C|
=C~  C�  C�  C���C�  C�
=C�C�  C�C�  C�C�C�  C���C�  C�
=C�C���C�  C�C�  C���C���C�C�
=C�  C�  C�  C�  C�C�  C���C���C�  C�  C�  C�C�C�C�  C�  C�  C���C���C���C���C�  C���C���C�  C�C�  C�  C�C�C�  C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C�  C���C�  C���C�  C�
=C�C�  C�  C�C���C�  C�  C���C���C�  C�  C���C�  C�  C���C�  C�C�C�C�C�  C�  C�  C�  C�  C���C�C�  C�C�C���C�  C�C�C�  C�  C�  C�C�C�C�  C���C�  C�  C�  C���C���C�C�
=C�
=C�
=C�  C���C���C�  C�
=D �D ��D�D��D  D��D�D��D�D� D�qD� D�qDz�D�qD��DD��D	  D	� D
  D
}qD  D��D  D� D�qD� D  D� D  D��D�D� D�qD}qD  D� D�D��D  D}qD�qD� D�qD� D  D� D�D��D�D� D�D� D�qD}qD�qD� D  D� D�D� D�qD� D�qD � D!  D!}qD"�D"� D"�qD#� D$  D$}qD%  D%}qD%�qD&� D'  D'� D(  D(� D)�D)� D*  D*� D+  D+� D,  D,� D-  D-� D-�qD.}qD/  D/� D0  D0��D1  D1��D2  D2� D3�D3� D3�qD4� D5  D5� D5�qD6}qD6�qD7� D8�D8}qD9  D9��D9�qD:}qD;  D;��D<�D<��D=D=� D=�qD>� D>�qD?� D@�D@� D@�qDA� DB  DB��DC�DC�DD�DD� DE�DE��DE�qDF� DG  DG}qDG�qDH}qDH�qDI� DJ  DJ��DK  DK}qDL  DL��DM�DM� DN  DN� DN�qDO}qDO��DP� DQ�DQ��DR�DR� DS  DS� DT�DT� DU  DU}qDV  DV��DW  DW��DX�DX� DY  DY��DY�qDZ� D[  D[��D\�D\� D\�qD]� D^  D^��D_�D_��D`  D`��DaDa� Da�qDb}qDb�qDc}qDd�Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di�qDj}qDk  Dk� Dl  Dl}qDl�qDm}qDm�qDnz�Dn��Doz�Do�qDp� Dq  Dq}qDr  Dr� Ds  Ds� Dt  Dt��Du  Du� Dv�Dv}qDw  Dw� Dx�Dx�Dy�Dy� Dz  Dz� Dz�qD{� D|�D|� D}  D}��D}�qD~� D~�qDz�D�  D�AHD��HD�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D��HD�D��D�B�D�~�D��qD���D�>�D�� D���D���D�@ D�� D���D�  D�@ D�~�D��HD�HD�AHD�� D���D�HD�@ D�� D���D�  D�@ D�� D�� D�  D�AHD�~�D�� D�  D�>�D�� D�D�HD�>�D�~�D��qD�  D�AHD��HD�� D�  D�>�D�}qD��qD��qD�@ D�� D���D�HD�AHD�� D�� D���D�>�D�� D�� D�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�=qD�}qD��qD��qD�=qD�}qD�� D�  D�=qD�~�D���D�  D�AHD�~�D�� D�  D�@ D��HD�� D�  D�AHD�~�D�� D�HD�@ D�� D���D���D�>�D�~�D�� D�  D�>�D�~�D��HD�HD�>�D�� D��HD�HD�AHD�~�D���D�  D�@ D�� D�� D�HD�B�D��HD��HD�HD�AHD��HD��HD�  D�@ D�~�D�� D�HD�AHD��HD��HD�  D�@ D��HD��HD���D�@ D�� D���D���D�>�D�~�D�� D�  D�>�D�� D�� D�  D�AHD�~�D���D�  D�AHD�� D���D�  D�@ D�� D��HD�HD�AHD���D�� D���D�@ D��HD�� D�  D�>�D�~�D���D�  D�@ D��HD�� D�  D�@ D�}qD���D�  D�AHD�~�D�� D�HD�AHD��HD�� D�  D�@ D�� D��HD���D�@ D�� D���D���D�>�D��HD�� D�  D�@ D��HD��HD�  D�AHD���D���D���D�AHD��HD��HD�HD�AHD��HD�D��D�B�D���D�D�HD�@ D�� D��HD�HD�>�D��HD�� D�  D�>�D�� D�D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?��?aG�?�\)?�33?�
=?�@\)@!G�@.{@@  @Q�@c�
@u@��\@���@�
=@�G�@���@���@�(�@Ǯ@�\)@ٙ�@��@�\)@�Q�A ��AA�A  Az�A��A\)A$z�A'�A.{A333A7�A<(�AAG�AG
=AL(�AQG�AU�AZ=qA`  Adz�AhQ�Amp�Ar�\Aw�A|(�A�  A��\A�p�A�\)A���A��
A��RA���A��HA��A�  A�=qA�(�A�ffA�G�A��A�p�A�  A��\A�z�A�
=A���A�33A�A�  A��A�z�A�
=A���AÅA�A�Q�Aʏ\A�p�A�\)Aљ�A�(�A�
=A�G�A�33A�p�A�Q�A�33A��A�A�=qA��A�  A�=qA���A��A��\A��A�\)B ��BffB�
B�B=qB\)B��B
=qB\)B��BB
=Bz�Bp�BffB33Bz�Bp�B=qB�HB�Bz�B�BB�\B\)B  B��B�B{B�HB\)B   B ��B!p�B"ffB#33B#�B$(�B%�B%�B&�\B'33B(  B(��B)p�B)�B*�RB+�B,Q�B,��B-��B.=qB/
=B0  B0��B1�B1�B2�RB3�B4(�B4��B5��B6�\B7\)B7�
B8��B9p�B:ffB;33B;�
B<z�B=G�B>{B?
=B?�B@Q�BA�BB{BB�HBC�BD(�BE�BF{BF�RBG\)BH  BH��BI�BJ�RBK\)BL  BL��BM�BN�RBO33BO�
BP��BQBRffBS
=BT  BT��BUBVffBW33BX  BX��BY��BZffB[33B\  B\��B]�B^�\B_\)B`Q�BaG�Bb{Bb�RBc�Bd��Be��Bf=qBg
=Bg�
Bh��BiBj�\Bk\)BlQ�Bmp�Bn=qBn�HBo�Bp��BqBr�RBs\)Bt(�Bu�Bv{Bw33Bw�
Bx��By��Bz�RB{�B|��B}p�B~=qB
=B�{B��\B�
=B�p�B��
B�=qB��RB�G�B�B�(�B��\B���B�p�B��B�z�B��HB�\)B�B�(�B��RB�33B���B�{B�z�B���B��B�  B�z�B��HB�G�B��B�=qB��HB�G�B��B�(�B���B�33B��B�{B��\B��B���B�(�B��RB��B���B�{B��RB�G�B��
B�ffB���B�p�B�  B�z�B�
=B��B�=qB��HB�p�B��B�ffB���B���B�=qB��HB�\)B��B��\B�
=B��
B�ffB���B��B�  B���B�G�B��
B�z�B��B�B�Q�B��HB�p�B�  B��\B�G�B��B��\B��B��B�=qB���B�p�B�{B��RB�\)B��B��\B��B��B�Q�B��HB��B�(�B��RB�\)B�  B��RB�\)B�  B��\B��B��B�ffB�
=B��B�Q�B��HB�p�B�  B\B�33B�B�z�B�
=BŮB�Q�B���BǅB�(�BȸRB�33B�B�Q�B���B˙�B�=qB���B�p�B�{BΏ\B��BϮB�Q�B��HBхB�(�B���B�p�B�  Bԣ�B��B�B�Q�B��HB�p�B�  B؏\B�33B�B�Q�B���BۅB�(�BܸRB�G�B��Bޏ\B��B߮B�Q�B���B�B�{B��B�33B�B�Q�B��HB�\)B�  B�\B��B癚B�=qB���B�\)B��B�z�B��B�B�=qB��HB�p�B�{B��B�33B�B�ffB��HB�B�  B��B�33B�B�Q�B���B�\)B�  B�z�B�
=B���B�(�B��RB�G�B��
B�ffB���B��B�{B���B��B��B�(�B���B��B��B��
C �C G�C p�C ��C �RC �
C ��C{C(�CG�C\)Cp�C�\C��C�RC��C�C{C(�CG�Cp�C�C��CC�HC��C{C33CQ�CffCz�C��CC�HC  C�C=qCffCz�C��CC�C
=C33CQ�Cp�C��CC�C
=C33C\)Cz�C��CC�C{C=qC\)C�C�C��C��C�CG�Cp�C�\C�RC�HC	
=C	33C	Q�C	z�C	�C	��C	��C
�C
G�C
p�C
��C
C
��C�CG�Cp�C��CC�C�CG�Cp�C��CC�C�CG�Cp�C��CC�C{C=qCffC��CC�C
=C33CffC�\C�RC�HC
=C33C\)C�C�RC�HC
=C33C\)C�C�RC�HC
=C=qCffC�\C�RC�HC
=C33CQ�Cz�C�C�
C
=C33C\)C�C��C�
C  C(�CQ�C�C�C�
C  C(�CQ�Cz�C�C��C��C�CG�Cp�C��CC�HC{C33C\)C�C�C�
C  C�CG�CffC�\CC�HC{C=qC\)C�C�C�
C  C(�CQ�C�C�C�
C  C(�CQ�Cz�C�C��C��C�CG�Cp�C��C�RC�HC
=C33C\)C�C�C��C��C�CG�Cp�C��CC�C 
=C 33C \)C z�C �C ��C ��C!�C!G�C!p�C!��C!C!�C"{C"=qC"ffC"�\C"�C"�HC#  C#33C#Q�C#z�C#��C#��C#�C$�C$=qC$ffC$�C$�C$�
C%  C%(�C%Q�C%z�C%�C%�
C&  C&(�C&Q�C&z�C&��C&C&�C'{C'=qC'\)C'�C'�RC'�HC(
=C(=qC(ffC(��C(C(�C){C)=qC)ffC)�\C)�C)�
C*
=C*33C*\)C*�\C*C*��C+�C+G�C+p�C+��C+��C+��C,�C,G�C,z�C,��C,�
C-
=C-=qC-ffC-��C-C-�C.�C.=qC.p�C.��C.C.��C/�C/Q�C/�C/�C/�HC0{C0=qC0p�C0�\C0C0�C1{C1=qC1p�C1��C1��C1��C2(�C2\)C2��C2��C2��C3(�C3\)C3�\C3�RC3�C4�C4Q�C4z�C4�C4�
C5
=C5=qC5p�C5��C5�HC6{C6G�C6z�C6�C6�HC7{C7G�C7z�C7��C7��C8  C833C8ffC8��C8��C9
=C9=qC9p�C9�C9�
C:
=C:=qC:ffC:��C:��C;
=C;=qC;z�C;�C;�C<�C<Q�C<�C<�RC<�C=�C=Q�C=z�C=�RC=�C>(�C>p�C>��C>�HC?(�C?\)C?��C?�
C@
=C@=qC@z�C@�RC@��CA=qCAz�CA�RCA��CB(�CBffCB��CB�
CC{CCQ�CC��CC�
CD{CDQ�CD�\CDCE  CE33CEp�CE�CE��CF33CFz�CF�RCF�CG(�CG\)CG��CG�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�A�A�  A���A�Q�A�A�jA�Q�A�C�A�=qA�7LA�1'A�+A�oA���A�-A�%A��A�\)AυA�ĜA���A�p�A�hsA�dZA�C�A��A̾wA̅A�XA��A��A˲-A�A�AʬA�A�O�A�O�A�VA�oA�v�A��mA�ZAİ!A�A�A��A��FA�9XA���A�oA�v�A�~�A��A��A��DA�1A�ƨA�JA�K�A���A�JA�bNA�33A���A�=qA��A�JA�bA���A��HA�A�ffA��A�JA��mA���A��PA� �A�O�A���A�A���A�dZA~�RAw�FAsoAm�TAhA�Ad��Ab��Aa+A^ȴA]+AZz�AW�AT�/ARbAP�AN��AL�\AJVAE�AD�/ADJAC��AB~�AB=qAA�^A@��A=�TA<~�A;�mA;G�A:�/A:v�A9O�A7�^A7C�A6�A5p�A4n�A1�-A/ƨA/33A.�A/A1�wA0��A0ĜA.A* �A)oA(��A'�mA'��A'\)A&�A&-A%�7A#��A"{A"��A v�A��A9XA5?AA�#A�PAK�A�9AI�A��A5?A  A�A��A��A��A��A^5A$�AA�Az�A��AȴAjA��A��AE�A�7Az�A{A  A�TA��A�A^5A�;Ax�Al�A�`A(�A�mA`BA�HA�jAjA��A��A\)A�yA�\AI�A�A�wA�hA|�A�7Ap�AȴA~�A�A�^A�A��Az�A(�A�#A?}A
�uA
1A	ƨA	dZA�/A�\AjA=qA(�A�AK�AĜAE�A�TA��A��A�mA�A�AffA�A  AƨA��At�A?}A
=A �/A �9A Q�@��P@�n�@��@�z�@�dZ@�hs@���@��@��w@�dZ@��R@��@�7L@�1'@�@�33@��@�ff@��@�?}@�@�b@�P@�n�@�J@���@��T@�hs@�@�9X@�ff@�7L@���@�@�bN@���@�P@�@噚@�z�@�I�@��m@��@��#@�`B@��`@��@�M�@�x�@�G�@��@���@۾w@ڗ�@��@�hs@�&�@�r�@��;@׮@��@�{@��#@�hs@�K�@���@�^5@�$�@ѡ�@�?}@���@Гu@�I�@�+@·+@��#@�X@���@�Q�@�
=@���@���@ʧ�@�E�@��T@�X@���@�1'@Ǖ�@�V@���@��@��`@ēu@å�@�@�p�@��@��@��@�C�@�@�{@���@�j@�Z@�Z@�Q�@�I�@�(�@��@���@��+@�5?@�J@��T@��-@��h@�hs@��u@��m@��w@��@�K�@���@�@��h@�&�@��`@�Ĝ@��D@�Z@��@�t�@�;d@���@��T@��-@�?}@�V@�V@��@�%@���@��@�bN@�ƨ@�K�@�33@��R@�n�@�5?@�@���@��h@�hs@�7L@��@���@��@�bN@���@�@�M�@��^@�?}@��/@���@��@��@�I�@�b@��w@��P@�|�@�@�n�@�-@�@��7@�O�@�?}@�&�@��9@�ƨ@�\)@���@���@�v�@�{@�@�X@�?}@�V@��/@��9@��D@�bN@��m@�dZ@�;d@�o@��@�n�@�-@���@��@��T@��-@�p�@�&�@���@��u@�(�@�ƨ@���@�|�@�S�@�@�ȴ@�~�@�-@��@���@�?}@��`@�I�@��;@���@�33@���@��@��R@���@�$�@��-@�X@�G�@�G�@�O�@��`@��u@���@���@�33@��H@�V@�{@��T@�@���@�?}@���@���@�b@���@��P@�C�@���@���@�X@�V@���@��m@���@�S�@�+@�o@���@�M�@�E�@�=q@��#@�X@��@�Ĝ@�Z@���@�33@��y@��R@�E�@��@��^@���@��7@�O�@��@���@��@�Q�@��m@��P@�dZ@�C�@�"�@��@���@���@��+@�v�@�5?@�@��-@�7L@��`@�Ĝ@���@��D@�9X@|�@~ȴ@}�@|z�@{��@{�F@{S�@{o@{@z�\@zJ@yX@x�@w�@w|�@w;d@w
=@v�R@v�+@vff@vE�@v@u@u�h@u`B@t�/@t�j@t�D@s��@r�H@rn�@r-@q��@q��@qx�@q7L@p�`@p�@pb@o�@ol�@o;d@nE�@mp�@m�@lZ@l9X@kƨ@k33@j��@i��@i&�@hbN@h1'@g�@gl�@f��@fv�@f{@e�@d��@d�@c��@c@b��@b~�@a�@ax�@a&�@`Ĝ@`A�@_��@_�@_l�@_;d@^��@^�R@^V@]�@]@]�h@\�@\��@\�D@[�
@[��@[dZ@[o@Z��@ZM�@Z-@Y�#@Y�7@Yhs@YG�@X��@XQ�@W�@W��@W|�@Wl�@W;d@V�y@V�+@V5?@U@Up�@U�@T�/@T�@S�
@SdZ@R��@R~�@R=q@Q��@Q��@P��@P�u@Pr�@P  @O��@O|�@O
=@N�@N�R@N��@Nv�@M@MV@L�j@L�D@K��@K��@Kt�@K33@J�\@J�@I�#@I�7@I&�@H��@HA�@H  @G��@G;d@G
=@Fȴ@F��@Fv�@FV@FE�@E�@E��@E?}@D�/@D�D@DI�@C��@CC�@B�H@B��@B�@A��@A7L@@��@@ �@?�@?K�@>�y@>��@>{@=��@=�h@=`B@=�@<��@<�@<��@<��@<z�@<1@;S�@:��@:�\@:�@9�^@9�7@9G�@9%@8��@8bN@7�@7��@7|�@7l�@7K�@6�@6E�@5�@5�T@5��@5�h@5?}@4��@4I�@41@3�m@3��@3��@3t�@3o@2�H@2��@2�!@2�!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A�%A�
=A���A���A�A�
=A���A���A�A�A�
=A�  A��A��
A�l�A։7A�oA��;A��#A�ȴAՅA�l�A�n�A�`BA�O�A�Q�A�Q�A�I�A�A�A�=qA�A�A�?}A�9XA�9XA�;dA�7LA�33A�1'A�1'A�/A�-A�+A�/A�1'A�-A�$�A�"�A�$�A�$�A� �A��A�{A�oA�%A��A��A��TA���AԺ^Aԩ�Aԝ�AԋDA�z�A�n�A�XA�+A�%A��/AӺ^AӇ+A�S�A�/A�A��;Aң�A�33Aѥ�A�O�A���A��TA���Aк^AЮAЛ�AЅA�ffA�O�A�/A�
=A��/A�Aϙ�A�|�A�bNA�I�A�=qA�/A�  A���AΟ�A�t�A�G�A�A��/AͼjAͺ^AͲ-A͍PA�v�A�n�A�n�A�p�A�n�A�jA�hsA�jA�l�A�l�A�hsA�`BA�^5A�\)A�VA�Q�A�VA�`BA�ffA�jA�x�A�|�A�r�A�l�A�hsA�hsA�ffA�^5A�Q�A�=qA�/A�(�A�$�A��A�VA�oA�oA�
=A�A���A���A���A��A���A���A���A���A���A���A���A���A�ȴA̴9A̩�A̬A̬A̧�A̡�A̓uA̍PA̋DẢ7Ȧ+A̅A�z�A�v�A�p�A�jA�ffA�bNA�bNA�^5A�\)A�XA�S�A�K�A�?}A�7LA�/A� �A�
=A���A��A��mA��;A���A���A���A���A���A���A���A���A��#A��HA��HA��;A��TA��`A��`A���A˴9AˬA˩�A˥�Aˡ�A˟�A˙�Aˉ7A�~�A�x�A�r�A�t�A�p�A�I�A��AʾwAʗ�AʋDAʇ+Aʕ�Aʣ�A���A��HAʶFAʲ-AʬAʕ�A�p�A�E�A��A�  A��mA���Aɡ�A�jA�XA�S�A�S�A�M�A�K�A�M�A�O�A�Q�A�O�A�M�A�K�A�M�A�O�A�Q�A�S�A�O�A�O�A�O�A�Q�A�S�A�VA�VA�Q�A�Q�A�S�A�XA�\)A�`BA�^5A�S�A�&�A���A��#A���A�ƨA�ĜAȼjAȍPA�z�A�p�A�l�A�ZA�?}A�1'A�+A��A�bA���A��`A���AǼjAǏ\A�bAƺ^Aƙ�A�n�A�ZA�XA�bAŴ9A�G�A�bA���A��;A���A�v�A�E�AöFA�bA�A�=qA�VA�A���A�|�A�hsA�ZA�C�A�&�A�
=A���A�dZA���A��hA��RA��A��A���A��FA���A��A�bNA�5?A��`A��uA�l�A�C�A��`A���A�G�A�oA���A��uA�l�A�7LA��A��RA�~�A�S�A�JA��
A�  A�G�A��+A�{A��
A���A��DA�n�A�\)A�?}A���A��A���A�M�A�"�A��A�oA�  A��A��A��A��yA��yA��mA��HA���A�ƨA��-A�jA��RA�?}A�{A�
=A�1A�  A��A��yA��/A���A���A�ĜA���A���A��DA�^5A�Q�A�5?A�JA��jA�v�A��mA�l�A�M�A�C�A�1'A�"�A�{A�JA�A��A��TA��uA�1'A�ĜA��DA�x�A�S�A�JA�ȴA��+A�ZA�(�A��TA��A�oA��A�ĜA���A�dZA�9XA���A���A��jA���A�hsA�E�A�"�A�A��TA���A�Q�A���A��9A���A��PA�hsA�ƨA�p�A�ZA�K�A�7LA�oA��/A���A�jA���A��hA��A��HA��#A���A��RA��+A�VA��A�1A�  A���A��A��TA���A�ĜA��A���A��\A�n�A�hsA�O�A�K�A�+A� �A��A�JA���A��
A���A��A�dZA�33A�%A��A��-A��hA�z�A�dZA�S�A�;dA�1'A�oA�%A��A��
A��hA��A�dZA�33A�JA��TA�A���A�hsA�(�A��`A���A�|�A�bNA�A�A��A���A���A��^A�ffA�+A��;A���A�\)A�+A���A��TA��A���A�ĜA��!A���A���A��\A��A�r�A�bNA�O�A�I�A�?}A�&�A�{A�JA�  A���A��A��HA���A��jA��A���A���A��uA�x�A�S�A�33A��A��A�|�A��A�ffA�
=A��HA���A���A��9A��!A��!A���A���A�|�A�~�A�^5A�9XA��yA��RA��A���A���A���A��\A�|�A�dZA�S�A�I�A�1'A�bA��#A���A���A�l�A�I�A�bA���A���A�hsA�1'A�
=A���A�p�A�/A��A���A�ȴA�dZA���A�-A�+A��A��/A�ȴA��FA��\A�M�A�O�A�/A�{A�`BA��A���A�ȴA�n�A��A�A���A��jA��A��uA�hsA�E�A��A�dZA��yA�ĜA���A��\A�|�A�t�A�bNA�9XA� �A��yA��-A�x�A�VA� �A���A��;A���A�/A��^A�7LA��jA�|�A�7LA��A�ZA���A��-A�|�A�I�A�%A��A�33AA�A~A�A}ƨA}��A}dZA|�yA|9XA{�FA{S�A{%Az�jAzffAy�Ay?}AwƨAuƨAu�At�yAt��At�jAt��At��At��At�\At�AtjAt^5At{As��Ar�\Ap�RApQ�Ap�Ap{Ao�Ao�wAo�PAo7LAnjAmAmS�Am+Am%Al�!Al(�Ak��Ak%Ajv�Ai��Ai�Ai�AhĜAhVAg��Ag��Ag�7AgXAf��AeAe�-Aep�AeXAe33Ae&�Ae�Ae�Ae%Ad�Ad��Ad��Adv�AdM�Ad1'AdJAc�Ac�FAcx�Ac;dAb��Ab��AbA�Aa�wAa�^Aa�FAa��Aa��Aa��Aa�7Aap�Aa\)AaK�Aa�AaA`��A`��A`n�A`JA_�^A_x�A_33A_A^��A^��A^jA^1'A]��A]�;A]�FA]��A]��A]��A]t�A]XA];dA]A\�/A\�!A\�A\^5A\5?A\�A[�FA[&�AZ��AZ-AY��AYK�AY33AY33AY/AY&�AY�AX��AW��AWt�AW�AVbNAU�AU�AU"�AU/AU/AU7LAU�AT��AT��AT�/ATȴATĜAT~�AS��AS;dAR�ARv�AQ�AQ��AQ��AQ�AQ��AQ�hAQ�PAQ�7AQ|�AQ\)AQO�AQVAP�HAPĜAP�\APbNAPM�AP=qAO�AO|�AO�AN�HAN��AN�DAN�ANz�ANjANbAMp�AMoALȴAL��ALVAL�AK�AK�-AK��AKhsAK"�AJ�AJ�\AJ=qAJ{AI�;AI�7AI;dAH�RAG\)AE�AE�7AE�PAE�AES�AD�AD��AD�jAD��AE;dAD��AD�AD��AD��AD~�ADE�AD�AD  AC�AC��AC��AD  AD  AC��AC��AC�AC�
AC�FAC��AC|�AC/AB��AB��AB�DAB�+AB~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�  A���A�Q�A�A�jA�Q�A�C�A�=qA�7LA�1'A�+A�oA���A�-A�%A��A�\)AυA�ĜA���A�p�A�hsA�dZA�C�A��A̾wA̅A�XA��A��A˲-A�A�AʬA�A�O�A�O�A�VA�oA�v�A��mA�ZAİ!A�A�A��A��FA�9XA���A�oA�v�A�~�A��A��A��DA�1A�ƨA�JA�K�A���A�JA�bNA�33A���A�=qA��A�JA�bA���A��HA�A�ffA��A�JA��mA���A��PA� �A�O�A���A�A���A�dZA~�RAw�FAsoAm�TAhA�Ad��Ab��Aa+A^ȴA]+AZz�AW�AT�/ARbAP�AN��AL�\AJVAE�AD�/ADJAC��AB~�AB=qAA�^A@��A=�TA<~�A;�mA;G�A:�/A:v�A9O�A7�^A7C�A6�A5p�A4n�A1�-A/ƨA/33A.�A/A1�wA0��A0ĜA.A* �A)oA(��A'�mA'��A'\)A&�A&-A%�7A#��A"{A"��A v�A��A9XA5?AA�#A�PAK�A�9AI�A��A5?A  A�A��A��A��A��A^5A$�AA�Az�A��AȴAjA��A��AE�A�7Az�A{A  A�TA��A�A^5A�;Ax�Al�A�`A(�A�mA`BA�HA�jAjA��A��A\)A�yA�\AI�A�A�wA�hA|�A�7Ap�AȴA~�A�A�^A�A��Az�A(�A�#A?}A
�uA
1A	ƨA	dZA�/A�\AjA=qA(�A�AK�AĜAE�A�TA��A��A�mA�A�AffA�A  AƨA��At�A?}A
=A �/A �9A Q�@��P@�n�@��@�z�@�dZ@�hs@���@��@��w@�dZ@��R@��@�7L@�1'@�@�33@��@�ff@��@�?}@�@�b@�P@�n�@�J@���@��T@�hs@�@�9X@�ff@�7L@���@�@�bN@���@�P@�@噚@�z�@�I�@��m@��@��#@�`B@��`@��@�M�@�x�@�G�@��@���@۾w@ڗ�@��@�hs@�&�@�r�@��;@׮@��@�{@��#@�hs@�K�@���@�^5@�$�@ѡ�@�?}@���@Гu@�I�@�+@·+@��#@�X@���@�Q�@�
=@���@���@ʧ�@�E�@��T@�X@���@�1'@Ǖ�@�V@���@��@��`@ēu@å�@�@�p�@��@��@��@�C�@�@�{@���@�j@�Z@�Z@�Q�@�I�@�(�@��@���@��+@�5?@�J@��T@��-@��h@�hs@��u@��m@��w@��@�K�@���@�@��h@�&�@��`@�Ĝ@��D@�Z@��@�t�@�;d@���@��T@��-@�?}@�V@�V@��@�%@���@��@�bN@�ƨ@�K�@�33@��R@�n�@�5?@�@���@��h@�hs@�7L@��@���@��@�bN@���@�@�M�@��^@�?}@��/@���@��@��@�I�@�b@��w@��P@�|�@�@�n�@�-@�@��7@�O�@�?}@�&�@��9@�ƨ@�\)@���@���@�v�@�{@�@�X@�?}@�V@��/@��9@��D@�bN@��m@�dZ@�;d@�o@��@�n�@�-@���@��@��T@��-@�p�@�&�@���@��u@�(�@�ƨ@���@�|�@�S�@�@�ȴ@�~�@�-@��@���@�?}@��`@�I�@��;@���@�33@���@��@��R@���@�$�@��-@�X@�G�@�G�@�O�@��`@��u@���@���@�33@��H@�V@�{@��T@�@���@�?}@���@���@�b@���@��P@�C�@���@���@�X@�V@���@��m@���@�S�@�+@�o@���@�M�@�E�@�=q@��#@�X@��@�Ĝ@�Z@���@�33@��y@��R@�E�@��@��^@���@��7@�O�@��@���@��@�Q�@��m@��P@�dZ@�C�@�"�@��@���@���@��+@�v�@�5?@�@��-@�7L@��`@�Ĝ@���@��D@�9X@|�@~ȴ@}�@|z�@{��@{�F@{S�@{o@{@z�\@zJ@yX@x�@w�@w|�@w;d@w
=@v�R@v�+@vff@vE�@v@u@u�h@u`B@t�/@t�j@t�D@s��@r�H@rn�@r-@q��@q��@qx�@q7L@p�`@p�@pb@o�@ol�@o;d@nE�@mp�@m�@lZ@l9X@kƨ@k33@j��@i��@i&�@hbN@h1'@g�@gl�@f��@fv�@f{@e�@d��@d�@c��@c@b��@b~�@a�@ax�@a&�@`Ĝ@`A�@_��@_�@_l�@_;d@^��@^�R@^V@]�@]@]�h@\�@\��@\�D@[�
@[��@[dZ@[o@Z��@ZM�@Z-@Y�#@Y�7@Yhs@YG�@X��@XQ�@W�@W��@W|�@Wl�@W;d@V�y@V�+@V5?@U@Up�@U�@T�/@T�@S�
@SdZ@R��@R~�@R=q@Q��@Q��@P��@P�u@Pr�@P  @O��@O|�@O
=@N�@N�R@N��@Nv�@M@MV@L�j@L�D@K��@K��@Kt�@K33@J�\@J�@I�#@I�7@I&�@H��@HA�@H  @G��@G;d@G
=@Fȴ@F��@Fv�@FV@FE�@E�@E��@E?}@D�/@D�D@DI�@C��@CC�@B�H@B��@B�@A��@A7L@@��@@ �@?�@?K�@>�y@>��@>{@=��@=�h@=`B@=�@<��@<�@<��@<��@<z�@<1@;S�@:��@:�\@:�@9�^@9�7@9G�@9%@8��@8bN@7�@7��@7|�@7l�@7K�@6�@6E�@5�@5�T@5��@5�h@5?}@4��@4I�@41@3�m@3��@3��@3t�@3o@2�H@2��@2�!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A�%A�
=A���A���A�A�
=A���A���A�A�A�
=A�  A��A��
A�l�A։7A�oA��;A��#A�ȴAՅA�l�A�n�A�`BA�O�A�Q�A�Q�A�I�A�A�A�=qA�A�A�?}A�9XA�9XA�;dA�7LA�33A�1'A�1'A�/A�-A�+A�/A�1'A�-A�$�A�"�A�$�A�$�A� �A��A�{A�oA�%A��A��A��TA���AԺ^Aԩ�Aԝ�AԋDA�z�A�n�A�XA�+A�%A��/AӺ^AӇ+A�S�A�/A�A��;Aң�A�33Aѥ�A�O�A���A��TA���Aк^AЮAЛ�AЅA�ffA�O�A�/A�
=A��/A�Aϙ�A�|�A�bNA�I�A�=qA�/A�  A���AΟ�A�t�A�G�A�A��/AͼjAͺ^AͲ-A͍PA�v�A�n�A�n�A�p�A�n�A�jA�hsA�jA�l�A�l�A�hsA�`BA�^5A�\)A�VA�Q�A�VA�`BA�ffA�jA�x�A�|�A�r�A�l�A�hsA�hsA�ffA�^5A�Q�A�=qA�/A�(�A�$�A��A�VA�oA�oA�
=A�A���A���A���A��A���A���A���A���A���A���A���A���A�ȴA̴9A̩�A̬A̬A̧�A̡�A̓uA̍PA̋DẢ7Ȧ+A̅A�z�A�v�A�p�A�jA�ffA�bNA�bNA�^5A�\)A�XA�S�A�K�A�?}A�7LA�/A� �A�
=A���A��A��mA��;A���A���A���A���A���A���A���A���A��#A��HA��HA��;A��TA��`A��`A���A˴9AˬA˩�A˥�Aˡ�A˟�A˙�Aˉ7A�~�A�x�A�r�A�t�A�p�A�I�A��AʾwAʗ�AʋDAʇ+Aʕ�Aʣ�A���A��HAʶFAʲ-AʬAʕ�A�p�A�E�A��A�  A��mA���Aɡ�A�jA�XA�S�A�S�A�M�A�K�A�M�A�O�A�Q�A�O�A�M�A�K�A�M�A�O�A�Q�A�S�A�O�A�O�A�O�A�Q�A�S�A�VA�VA�Q�A�Q�A�S�A�XA�\)A�`BA�^5A�S�A�&�A���A��#A���A�ƨA�ĜAȼjAȍPA�z�A�p�A�l�A�ZA�?}A�1'A�+A��A�bA���A��`A���AǼjAǏ\A�bAƺ^Aƙ�A�n�A�ZA�XA�bAŴ9A�G�A�bA���A��;A���A�v�A�E�AöFA�bA�A�=qA�VA�A���A�|�A�hsA�ZA�C�A�&�A�
=A���A�dZA���A��hA��RA��A��A���A��FA���A��A�bNA�5?A��`A��uA�l�A�C�A��`A���A�G�A�oA���A��uA�l�A�7LA��A��RA�~�A�S�A�JA��
A�  A�G�A��+A�{A��
A���A��DA�n�A�\)A�?}A���A��A���A�M�A�"�A��A�oA�  A��A��A��A��yA��yA��mA��HA���A�ƨA��-A�jA��RA�?}A�{A�
=A�1A�  A��A��yA��/A���A���A�ĜA���A���A��DA�^5A�Q�A�5?A�JA��jA�v�A��mA�l�A�M�A�C�A�1'A�"�A�{A�JA�A��A��TA��uA�1'A�ĜA��DA�x�A�S�A�JA�ȴA��+A�ZA�(�A��TA��A�oA��A�ĜA���A�dZA�9XA���A���A��jA���A�hsA�E�A�"�A�A��TA���A�Q�A���A��9A���A��PA�hsA�ƨA�p�A�ZA�K�A�7LA�oA��/A���A�jA���A��hA��A��HA��#A���A��RA��+A�VA��A�1A�  A���A��A��TA���A�ĜA��A���A��\A�n�A�hsA�O�A�K�A�+A� �A��A�JA���A��
A���A��A�dZA�33A�%A��A��-A��hA�z�A�dZA�S�A�;dA�1'A�oA�%A��A��
A��hA��A�dZA�33A�JA��TA�A���A�hsA�(�A��`A���A�|�A�bNA�A�A��A���A���A��^A�ffA�+A��;A���A�\)A�+A���A��TA��A���A�ĜA��!A���A���A��\A��A�r�A�bNA�O�A�I�A�?}A�&�A�{A�JA�  A���A��A��HA���A��jA��A���A���A��uA�x�A�S�A�33A��A��A�|�A��A�ffA�
=A��HA���A���A��9A��!A��!A���A���A�|�A�~�A�^5A�9XA��yA��RA��A���A���A���A��\A�|�A�dZA�S�A�I�A�1'A�bA��#A���A���A�l�A�I�A�bA���A���A�hsA�1'A�
=A���A�p�A�/A��A���A�ȴA�dZA���A�-A�+A��A��/A�ȴA��FA��\A�M�A�O�A�/A�{A�`BA��A���A�ȴA�n�A��A�A���A��jA��A��uA�hsA�E�A��A�dZA��yA�ĜA���A��\A�|�A�t�A�bNA�9XA� �A��yA��-A�x�A�VA� �A���A��;A���A�/A��^A�7LA��jA�|�A�7LA��A�ZA���A��-A�|�A�I�A�%A��A�33AA�A~A�A}ƨA}��A}dZA|�yA|9XA{�FA{S�A{%Az�jAzffAy�Ay?}AwƨAuƨAu�At�yAt��At�jAt��At��At��At�\At�AtjAt^5At{As��Ar�\Ap�RApQ�Ap�Ap{Ao�Ao�wAo�PAo7LAnjAmAmS�Am+Am%Al�!Al(�Ak��Ak%Ajv�Ai��Ai�Ai�AhĜAhVAg��Ag��Ag�7AgXAf��AeAe�-Aep�AeXAe33Ae&�Ae�Ae�Ae%Ad�Ad��Ad��Adv�AdM�Ad1'AdJAc�Ac�FAcx�Ac;dAb��Ab��AbA�Aa�wAa�^Aa�FAa��Aa��Aa��Aa�7Aap�Aa\)AaK�Aa�AaA`��A`��A`n�A`JA_�^A_x�A_33A_A^��A^��A^jA^1'A]��A]�;A]�FA]��A]��A]��A]t�A]XA];dA]A\�/A\�!A\�A\^5A\5?A\�A[�FA[&�AZ��AZ-AY��AYK�AY33AY33AY/AY&�AY�AX��AW��AWt�AW�AVbNAU�AU�AU"�AU/AU/AU7LAU�AT��AT��AT�/ATȴATĜAT~�AS��AS;dAR�ARv�AQ�AQ��AQ��AQ�AQ��AQ�hAQ�PAQ�7AQ|�AQ\)AQO�AQVAP�HAPĜAP�\APbNAPM�AP=qAO�AO|�AO�AN�HAN��AN�DAN�ANz�ANjANbAMp�AMoALȴAL��ALVAL�AK�AK�-AK��AKhsAK"�AJ�AJ�\AJ=qAJ{AI�;AI�7AI;dAH�RAG\)AE�AE�7AE�PAE�AES�AD�AD��AD�jAD��AE;dAD��AD�AD��AD��AD~�ADE�AD�AD  AC�AC��AC��AD  AD  AC��AC��AC�AC�
AC�FAC��AC|�AC/AB��AB��AB�DAB�+AB~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B	oB	�B	 B	bB	�B	�B	
=B	
�B	DB	
rB	
rB		�B		lB	fB	JB	�B	�B	=�B	J�B	k�B	��B	ٴB	�B	�B
B
$@B
"�B
"�B
&LB
,qB
;�B
G�B
^jB
c�B
� B
��B
��B
��B
��B
�jB
��B�BNB�(B�B��BDB�B�BVB�B��B�B�B��BDB�BB 'B=B(B�B�B��B�gB�UB�qB��B��B|�B]dB2�BuB
�|B
�B
��B
�B
��B
��B
e�B
G�B
&B	�DB	�3B	�uB	n�B	GB	#�B	�B	�B��B�MB��B�,B�B�KB�BBݘB�B��B�>B	�B	0UB	@�B	DgB	T�B	e�B	sB	��B	ɆB	��B	� B	�B	��B	��B
	�B
7B
�B
�B
B
�B
�B	�B	��B	��B
	�B
;0B
<�B
8B
1[B
B	�]B	��B	�B	�xB
 �B
�B

=B
	B
B	��B
@B
�B
�B
�B
	B
fB
	�B

�B
B
(B
�B
�B
�B
4�B
B�B
D�B
GzB
J�B
S�B
X�B
X�B
YB
P�B
P�B
Y�B
\�B
\)B
Z�B
^jB
YB
R�B
MB
HKB
J#B
K)B
K�B
L0B
J�B
G�B
D�B
NpB
OBB
H�B
H�B
G�B
IB
K�B
J#B
I�B
H�B
I�B
J�B
K�B
M�B
N�B
OBB
O�B
O�B
Q�B
XEB
T�B
T�B
PHB
OB
N<B
MB
L�B
M�B
P}B
OvB
M�B
H�B
G�B
G�B
GB
F�B
FtB
F?B
E�B
E�B
FtB
D3B
CaB
B�B
B�B
B�B
>�B
<�B
?B
>�B
=�B
=<B
<�B
<6B
;�B
;0B
:^B
9�B
9XB
8RB
5�B
5�B
3�B
1�B
5?B
1�B
/�B
0!B
/�B
.�B
.IB
,�B
,B
+6B
*0B
)*B
(�B
(�B
'�B
&�B
'B
&B
%�B
&B
$tB
#�B
#�B
#�B
"�B
!�B
#�B
 'B
�B
�B
!B
�B
OB
�B
B
B
=B
qB
�B
�B
eB
�B
1B
�B
{B
�B
�B
:B
B
4B
 B
bB
bB
.B
(B
�B
\B
�B
�B
�B
~B
xB

�B

=B
	�B
	7B
�B
�B
�B
�B
�B
�B
�B
�B
+B
�B
B
SB
�B
%B
�B
�B
�B
�B
�B
+B
�B
�B

=B

�B

�B
	�B
fB
%B
YB
�B
YB
�B
1B
	�B
xB
~B
�B
�B
"B
�B
�B
�B
�B
4B
 B
 B
 B
�B
bB
�B
 B
4B
�B
�B
�B
4B
4B
hB
hB
4B
hB
hB
�B
hB
4B
�B
�B
:B
B
�B
B
B
�B
�B
�B
�B
YB
YB
�B
_B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
_B
�B
�B
{B
�B
B
�B
:B
B
FB
�B
�B
�B
�B
�B
YB
B
�B
�B
�B
�B
SB
SB
�B
�B
�B
�B
�B
�B
YB
+B
_B
�B
+B
�B
+B
�B
�B
�B
�B
�B
�B
1B
�B
�B
B
B
�B
�B
�B
B
�B
B
CB
�B
�B
xB
�B
B
IB
~B
B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
VB
�B
 \B
 'B
 �B
!bB
"4B
#:B
"�B
#:B
!�B
!�B
"�B
"�B
"hB
"hB
"hB
"�B
#B
#:B
$�B
%zB
&�B
'�B
'�B
)�B
)�B
+B
*�B
*�B
+�B
*�B
*eB
*eB
*eB
+�B
-CB
,�B
,�B
-�B
.IB
.�B
/OB
/�B
0UB
/�B
.�B
/OB
0�B
0!B
1'B
2aB
2�B
2�B
2�B
2�B
3�B
2�B
3�B
3�B
3�B
49B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5?B
6B
6FB
6B
6B
5�B
6FB
7LB
7LB
8�B
9XB
9XB
9XB
9XB
9XB
9$B
9�B
9�B
:�B
:�B
;�B
;�B
<B
;�B
<6B
<�B
<�B
=B
=�B
=qB
=�B
>BB
=�B
=<B
=B
=�B
>BB
>wB
>�B
>wB
>�B
>�B
>�B
?B
?HB
?}B
?}B
?�B
?}B
A�B
@�B
A�B
A�B
AUB
A�B
B'B
B�B
B�B
C�B
C�B
C-B
C�B
CaB
D3B
C�B
C�B
DgB
D�B
EmB
E�B
E�B
FB
FB
FtB
F�B
F�B
GB
GEB
G�B
G�B
G�B
HB
H�B
H�B
IRB
IRB
IRB
IRB
I�B
I�B
I�B
J�B
J�B
J�B
K^B
K�B
K�B
K�B
L0B
L0B
K�B
K�B
L0B
LdB
L�B
MB
L�B
L�B
L�B
M6B
MjB
M�B
NB
NB
NpB
NpB
NpB
OBB
OvB
O�B
PB
PHB
PHB
P}B
QB
QB
QNB
Q�B
Q�B
Q�B
R�B
RTB
R�B
RTB
RTB
S&B
S[B
S�B
S�B
T,B
T,B
T,B
T,B
T�B
T�B
U2B
U2B
U�B
U�B
V9B
VB
V�B
V�B
V�B
V�B
W
B
V�B
W
B
W
B
W?B
WsB
W�B
W�B
XB
XB
X�B
X�B
X�B
YKB
YKB
YKB
ZB
ZB
Z�B
ZQB
[#B
[#B
[#B
[�B
[�B
[�B
[�B
\)B
\)B
\)B
\]B
\)B
\)B
\�B
]/B
]/B
]dB
^B
^5B
^jB
^�B
^�B
_B
_B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
`�B
`�B
`�B
aB
aHB
bB
a�B
bB
bNB
bB
bNB
b�B
b�B
b�B
c B
c G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	B	�B	�B	:B	�B	�B	�B	�B	\B	B	�B	�B	B	4B	�B	�B	+B	 4B	�B	�B	_B	�B	�B		7B	fB	�B	B	
	B	
�B	B	JB	�B		�B	
	B	DB	B		lB	
rB	xB	DB		lB	�B		7B		�B		�B	fB		lB	DB		�B	1B	�B	�B		B	1B	�B	1B	�B		lB	
=B	�B	~B	�B	�B	�B	B	�B	�B	�B	B	�B	B	�B	�B	CB	�B	�B	#nB	2�B	=<B	@OB	@OB	<jB	>B	?B	@�B	B�B	DgB	J�B	J�B	RTB	VB	[WB	`B	gB	iyB	tB	xlB	~�B	�MB	�B	�~B	��B	�tB	�tB	��B	�?B	�)B	�#B	�B	��B	�B	�TB	�B	�|B	�|B	�TB	�`B	��B	�,B	�,B	�mB	�KB	��B	�iB	��B	�B	��B	�lB	�B
�B
�B
�B
�B
�B
OB
$�B
%B
%�B
'�B
'�B
%FB
#nB
"4B
$�B
#B
#:B
#B
$B
$tB
#:B
!bB
 �B
#nB
$tB
 �B
�B
!�B
"4B
#:B
"�B
!bB
"hB
%�B
$B
"4B
!�B
"�B
$�B
&�B
$�B
$�B
%zB
&B
&LB
&�B
&LB
(�B
)_B
*eB
*�B
)�B
*0B
+6B
+�B
-CB
.�B
1�B
3�B
5?B
6FB
9�B
:^B
;�B
;0B
<jB
?B
A�B
B'B
A�B
B[B
B'B
C�B
E�B
H�B
F�B
J�B
L�B
M6B
MjB
NpB
X�B
b�B
bB
`B
`�B
b�B
bNB
bB
_�B
bNB
`�B
`�B
`vB
^�B
iB
l�B
h�B
f�B
h>B
iyB
g�B
r�B
�{B
��B
��B
��B
�B
�B
��B
�~B
�xB
��B
�MB
��B
��B
��B
��B
�1B
�7B
��B
�'B
��B
��B
��B
��B
�-B
��B
��B
�B
�zB
�B
�B
�B
��B
��B
��B
�B
�B
�XB
��B
��B
��B
��B
�CB
�B
ȴB
��B
�gB
خB
�]B
�dB
�5B
�B
�rBBAB�B+B�B"B�B�B4B�B{B�B$B�B6FB6zB;�BEBE�BW�BcTBpoBxlBx�Bv`B|�B�	B�bB��B�jB�B��B��B�B�]B��B�B��B�B�B��B�5B��B�]B�B{B�B�BVB�B�B�B
�B�B�B�B�B�BB�B�B�B�B�B.B"B�BB�BDB�B	�BAB(XB4�B�VB�;B�B�8B�|B�B�NB�B�rB�B��B�/B��B�`B�B�B��B�sB�B�B�yB�B�"B�B�)B�5BoB�BhB�B	lB�B�B
rB
=B�B�B�B
rB
�B{B�BYB$B+BxB!�B%B2�B&�B�BxBB=B�B�B�B�B�B'RB�B"�B(B�B\B�B�BPB�B
	B�BbB�BB�.B�B
rB�B�ZB�%B�oB�AB�B�B��B�B�B��B�WB�BB�yB�B�aB�?B�>B�BB�3B��B�OB��B�[B��B�BB��B�qB��B�$B�nB�4B��B�@B��B�B�YB��B��B�B��B�VB��B�"B��B��B��B�%B�B�B�B}"B{�B�B}�By�ByrBu%Bt�BuZBo5BncBk�Bh�Be�Bc�B`�B`�B^jB\)B[WBZ�BT,B^jBR BR BK)BL�BIBI�BFBE�BB�BC�B9�B5�B1�B.�B-�B)*B(�B!-B �BBOBB�B
=B%B 4B
��B
�]B
��B
�.B
��B
��B
��B
��B
�>B
�DB
�>B
��B
��B
�%B
��B
�B
��B
��B
�B
�)B
��B
�DB
�B
�B
�B
�B
��B
�B
ߤB
�WB
یB
�fB
�B
�NB
�HB
��B
�HB
�}B
�0B
�^B
��B
�XB
�B
�6B
�tB
�tB
��B
��B
��B
�=B
�*B
��B
�XB
��B
��B
��B
�B
�B
�LB
�-B
��B
��B
�~B
��B
�=B
��B
�$B
��B
�\B
�B
��B
� B
��B
��B
��B
�YB
��B
�4B
��B
��B
p;B
u�B
xlB
o�B
o�B
u%B
hsB
iB
k�B
iB
�%B
X�B
V�B
W
B
kB
S�B
N�B
ZQB
K�B
K^B
LdB
GzB
F?B
K)B
PHB
A B
5�B
5�B
/�B
0�B
-B
,B
2�B
'�B
'�B
(�B
 �B
	B
�B
�B
�B
{B
�B
"B
�B
B	�B	�|B	�B	�B	�yB	�B	�/B	�EB	�KB	ٴB	��B	�dB	�B	�[B	��B	�B	��B	�B	��B	�kB	�tB	�OB	�IB	�B	��B	�OB	�B	�4B	�oB	}�B	{�B	x�B	w�B	u�B	tB	tB	s�B	s�B	pB	s�B	s�B	�;B	uZB	^jB	U�B	S�B	T,B	S�B	OvB	S�B	VmB	OB	B�B	<6B	8�B	<jB	<�B	9XB	4�B	0!B	)�B	'RB	&�B	#�B	(�B	�B	�B	�B	YB	&LB	OB	VB	 B	�B	�B	�B	�B	B	JB		�B	�B	B	fB	fB	%B	�B	B	fB	�B	�B	�B	�B		�B	�B�DB�DB�B�rB��B�rB�xB�>B�	B��B�B�>B�fB�fB�	B��B�fB�%B�AB�vB�B�;B�vB�oB��B�5B��B��B�B��B�DB�B�"B�
B��B�B�>B��B�B�QB�"B�B�B�B�NB��BܒB�)B�]B�WB�)B�B�jB�B�B�B�B��B�B�yB�
B�)B��B�?B��B�gB�mB��B��B�B�B�|B�B��B�B��B�#BܒBیB�#B�#B��B��B�BBݘBܒB�B�B�/B��B��B�mBޞB�B�B��BیB�#B��B��B�B�,B��B�&B��B��B�B�TB�vB�`B�B��B�B�B�`B��B�B��B��B	B	(�B	MB	�B	�B	"�B	$@B	�B	�B	FB	.IB	6B	8RB	:�B	=�B	>�B	A B	B[B	?HB	@�B	@�B	B'B	@�B	B�B	B'B	B�B	CaB	B�B	C�B	C-B	D3B	JXB	HKB	LdB	J�B	QB	T�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	[B	�B	~B	�B	�B	DB	�B	
�B	
�B	
#B	
#B	
XB	�B	�B	*B	A�B	O�B	poB	�B	�WB	��B	�B
�B
%�B
#�B
$&B
'�B
.�B
<jB
IB
`�B
g�B
��B
��B
��B
��B
��B
�B BBW�B��B�B��BB1B�BOBjB�GB�B�=B B6B�B%zB$�B%FB�BgB�B�B�B�B�GB��B��B��Bm]BA�B�B
�^B
�B
�'B
�B
��B
�gB
v`B
V�B
<�B
�B	ڠB	�:B	�B	YB	.}B	�B		�B	�B�$B�B�oB��B�B�tB�B�RB�B�zB	IB	3B	BuB	H1B	U�B	g�B	wB	��B	͟B	��B	�'B	�rB	�(B
-B
B
�B
�B
!-B
eB
�B
�B	�zB	�<B	��B
MB
<�B
>B
A�B
=�B
�B
 OB	��B	�hB	�B
uB
(B
B
(B
	�B	�-B
7B
�B
MB
�B
	�B
	B

�B
�B
B
�B
}B
�B
FB
2�B
B[B
D�B
G�B
J�B
T�B
Y�B
Z�B
\�B
Q�B
O�B
Y�B
^B
_B
]/B
`�B
[�B
VSB
NpB
H�B
J�B
LJB
M�B
N�B
LdB
IB
E9B
PHB
Q�B
I�B
J�B
IlB
I�B
MB
K�B
KB
I�B
K�B
K�B
L�B
N�B
O�B
O�B
P.B
O�B
RTB
ZQB
U�B
VSB
Q4B
O�B
P.B
N�B
NB
N�B
R�B
Q�B
O�B
J	B
IB
I�B
H1B
GzB
GB
F�B
F�B
HB
HfB
FB
D�B
C�B
E�B
F%B
@4B
>]B
A�B
?�B
>]B
>B
=VB
<�B
<jB
;�B
;B
:xB
:�B
:^B
8B
88B
5%B
49B
8�B
3B
0�B
1vB
0�B
/�B
/�B
.B
-�B
,=B
+B
)�B
)�B
)�B
(sB
'�B
(>B
'8B
'�B
&�B
$�B
$&B
$�B
$�B
#�B
%,B
%�B
 �B
 \B
 'B
�B
 \B
�B
 �B
 B
�B
)B
�B
CB
�B
kB
QB
CB
B
�B
B
�B
FB
,B
oB
B
 B
�B
4B
�B
�B
�B
B
B
:B
�B
JB
)B
)B

�B

	B
	B
�B
�B
+B
B
�B
�B
B
	lB
?B
SB
�B
?B
�B
�B
�B
%B
B
	�B
�B
	RB
�B

�B
dB
�B
�B
	�B
EB
_B
B
�B
fB

rB

�B
�B
~B
�B
B
pB
pB
�B
�B
oB
�B
NB
hB
NB
4B
�B
B
hB
�B
TB
�B
B
 B
B
�B
�B
�B
�B
:B
�B
�B
TB
@B
B
B
uB
�B
B
MB
�B
�B
9B

B
+B
�B
yB
�B
�B
�B
1B
1B
KB
1B
KB
KB
B
�B
�B
$B
9B
�B
�B
�B
B
:B
&B
�B
2B
SB
�B
�B
�B
_B
�B
�B

B
SB
B
�B
?B
B
�B
9B
$B
$B
�B
�B
�B
�B
+B
�B
�B
yB
+B
yB
B
�B
B
KB
B
QB
B
B
B
7B
WB
CB
�B
)B
�B
�B
�B
�B
�B
IB
�B
�B
B
�B
�B
�B
�B
�B
 �B
 B
 �B
 \B
 BB
 B
�B
 'B
 �B
 �B
 \B
 �B
!bB
#B
#�B
#�B
#�B
"hB
"�B
#�B
# B
"�B
"�B
"�B
#TB
#�B
#�B
%�B
%�B
'mB
(sB
)B
+B
+B
+�B
+kB
,�B
,=B
+QB
*�B
*�B
*�B
,�B
-]B
,�B
-wB
.�B
/B
/B
0!B
0�B
1'B
0!B
/iB
0;B
1[B
0�B
1[B
2�B
3hB
3�B
3�B
3�B
49B
3�B
4�B
4B
4B
4�B
4�B
5B
5B
5%B
5B
5%B
5B
5ZB
6+B
6�B
6�B
6`B
6FB
6�B
72B
8B
8�B
9�B
9�B
9�B
9�B
9�B
9rB
9�B
:DB
:xB
;�B
;dB
<B
;�B
<6B
;�B
<jB
<�B
=B
=VB
=�B
=�B
>B
>�B
>B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?.B
?.B
?}B
?�B
?�B
@ B
@ B
@iB
B[B
@�B
B�B
A�B
A�B
B[B
B�B
CGB
C�B
DMB
C�B
C�B
DB
C�B
D�B
D3B
D�B
D�B
EmB
E�B
FtB
E�B
FYB
F�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
HB
HfB
H�B
IRB
I�B
I�B
I�B
I�B
J#B
I�B
JrB
KB
J�B
K)B
K�B
LJB
L0B
LJB
L~B
LJB
K�B
LJB
L�B
L�B
MB
MB
L�B
MB
MB
M�B
M�B
NB
NVB
NVB
N�B
N�B
OBB
O�B
PB
P.B
PbB
P�B
P�B
Q4B
Q�B
QNB
Q�B
R B
R B
RTB
R�B
RoB
R�B
R�B
SB
S�B
S�B
S�B
T,B
T{B
TaB
T{B
T�B
UgB
UMB
U�B
U�B
U�B
VB
V�B
VmB
W
B
V�B
W
B
W
B
W$B
V�B
W$B
WYB
W�B
W�B
XB
X+B
X_B
X�B
Y1B
YB
Y1B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
[qB
[�B
\B
\B
\)B
\CB
\CB
\CB
\CB
\�B
\]B
\�B
]IB
]�B
]~B
]�B
^jB
^jB
^�B
^�B
_;B
_VB
_pB
`'B
_�B
_�B
_�B
`\B
`�B
`�B
`�B
`�B
`�B
a-B
abB
a�B
bhB
bB
bNB
bhB
bNB
b�B
b�B
cB
b�B
c G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	B	�B	�B	:B	�B	�B	�B	�B	\B	B	�B	�B	B	4B	�B	�B	+B	 4B	�B	�B	_B	�B	�B		7B	fB	�B	B	
	B	
�B	B	JB	�B		�B	
	B	DB	B		lB	
rB	xB	DB		lB	�B		7B		�B		�B	fB		lB	DB		�B	1B	�B	�B		B	1B	�B	1B	�B		lB	
=B	�B	~B	�B	�B	�B	B	�B	�B	�B	B	�B	B	�B	�B	CB	�B	�B	#nB	2�B	=<B	@OB	@OB	<jB	>B	?B	@�B	B�B	DgB	J�B	J�B	RTB	VB	[WB	`B	gB	iyB	tB	xlB	~�B	�MB	�B	�~B	��B	�tB	�tB	��B	�?B	�)B	�#B	�B	��B	�B	�TB	�B	�|B	�|B	�TB	�`B	��B	�,B	�,B	�mB	�KB	��B	�iB	��B	�B	��B	�lB	�B
�B
�B
�B
�B
�B
OB
$�B
%B
%�B
'�B
'�B
%FB
#nB
"4B
$�B
#B
#:B
#B
$B
$tB
#:B
!bB
 �B
#nB
$tB
 �B
�B
!�B
"4B
#:B
"�B
!bB
"hB
%�B
$B
"4B
!�B
"�B
$�B
&�B
$�B
$�B
%zB
&B
&LB
&�B
&LB
(�B
)_B
*eB
*�B
)�B
*0B
+6B
+�B
-CB
.�B
1�B
3�B
5?B
6FB
9�B
:^B
;�B
;0B
<jB
?B
A�B
B'B
A�B
B[B
B'B
C�B
E�B
H�B
F�B
J�B
L�B
M6B
MjB
NpB
X�B
b�B
bB
`B
`�B
b�B
bNB
bB
_�B
bNB
`�B
`�B
`vB
^�B
iB
l�B
h�B
f�B
h>B
iyB
g�B
r�B
�{B
��B
��B
��B
�B
�B
��B
�~B
�xB
��B
�MB
��B
��B
��B
��B
�1B
�7B
��B
�'B
��B
��B
��B
��B
�-B
��B
��B
�B
�zB
�B
�B
�B
��B
��B
��B
�B
�B
�XB
��B
��B
��B
��B
�CB
�B
ȴB
��B
�gB
خB
�]B
�dB
�5B
�B
�rBBAB�B+B�B"B�B�B4B�B{B�B$B�B6FB6zB;�BEBE�BW�BcTBpoBxlBx�Bv`B|�B�	B�bB��B�jB�B��B��B�B�]B��B�B��B�B�B��B�5B��B�]B�B{B�B�BVB�B�B�B
�B�B�B�B�B�BB�B�B�B�B�B.B"B�BB�BDB�B	�BAB(XB4�B�VB�;B�B�8B�|B�B�NB�B�rB�B��B�/B��B�`B�B�B��B�sB�B�B�yB�B�"B�B�)B�5BoB�BhB�B	lB�B�B
rB
=B�B�B�B
rB
�B{B�BYB$B+BxB!�B%B2�B&�B�BxBB=B�B�B�B�B�B'RB�B"�B(B�B\B�B�BPB�B
	B�BbB�BB�.B�B
rB�B�ZB�%B�oB�AB�B�B��B�B�B��B�WB�BB�yB�B�aB�?B�>B�BB�3B��B�OB��B�[B��B�BB��B�qB��B�$B�nB�4B��B�@B��B�B�YB��B��B�B��B�VB��B�"B��B��B��B�%B�B�B�B}"B{�B�B}�By�ByrBu%Bt�BuZBo5BncBk�Bh�Be�Bc�B`�B`�B^jB\)B[WBZ�BT,B^jBR BR BK)BL�BIBI�BFBE�BB�BC�B9�B5�B1�B.�B-�B)*B(�B!-B �BBOBB�B
=B%B 4B
��B
�]B
��B
�.B
��B
��B
��B
��B
�>B
�DB
�>B
��B
��B
�%B
��B
�B
��B
��B
�B
�)B
��B
�DB
�B
�B
�B
�B
��B
�B
ߤB
�WB
یB
�fB
�B
�NB
�HB
��B
�HB
�}B
�0B
�^B
��B
�XB
�B
�6B
�tB
�tB
��B
��B
��B
�=B
�*B
��B
�XB
��B
��B
��B
�B
�B
�LB
�-B
��B
��B
�~B
��B
�=B
��B
�$B
��B
�\B
�B
��B
� B
��B
��B
��B
�YB
��B
�4B
��B
��B
p;B
u�B
xlB
o�B
o�B
u%B
hsB
iB
k�B
iB
�%B
X�B
V�B
W
B
kB
S�B
N�B
ZQB
K�B
K^B
LdB
GzB
F?B
K)B
PHB
A B
5�B
5�B
/�B
0�B
-B
,B
2�B
'�B
'�B
(�B
 �B
	B
�B
�B
�B
{B
�B
"B
�B
B	�B	�|B	�B	�B	�yB	�B	�/B	�EB	�KB	ٴB	��B	�dB	�B	�[B	��B	�B	��B	�B	��B	�kB	�tB	�OB	�IB	�B	��B	�OB	�B	�4B	�oB	}�B	{�B	x�B	w�B	u�B	tB	tB	s�B	s�B	pB	s�B	s�B	�;B	uZB	^jB	U�B	S�B	T,B	S�B	OvB	S�B	VmB	OB	B�B	<6B	8�B	<jB	<�B	9XB	4�B	0!B	)�B	'RB	&�B	#�B	(�B	�B	�B	�B	YB	&LB	OB	VB	 B	�B	�B	�B	�B	B	JB		�B	�B	B	fB	fB	%B	�B	B	fB	�B	�B	�B	�B		�B	�B�DB�DB�B�rB��B�rB�xB�>B�	B��B�B�>B�fB�fB�	B��B�fB�%B�AB�vB�B�;B�vB�oB��B�5B��B��B�B��B�DB�B�"B�
B��B�B�>B��B�B�QB�"B�B�B�B�NB��BܒB�)B�]B�WB�)B�B�jB�B�B�B�B��B�B�yB�
B�)B��B�?B��B�gB�mB��B��B�B�B�|B�B��B�B��B�#BܒBیB�#B�#B��B��B�BBݘBܒB�B�B�/B��B��B�mBޞB�B�B��BیB�#B��B��B�B�,B��B�&B��B��B�B�TB�vB�`B�B��B�B�B�`B��B�B��B��B	B	(�B	MB	�B	�B	"�B	$@B	�B	�B	FB	.IB	6B	8RB	:�B	=�B	>�B	A B	B[B	?HB	@�B	@�B	B'B	@�B	B�B	B'B	B�B	CaB	B�B	C�B	C-B	D3B	JXB	HKB	LdB	J�B	QB	T�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<O9=<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<1<EQ<y&d<#�
<|?<6�<>��<F��<�ѫ<��X<Yȳ<#�
<#�
<0��<#�
<#�
<>��<#�
<F��<P8<iLL<4h�<C�]<5��<P8<#�
<#�
<#�
<q�(<�ѫ<�k<#�
<*�3<n>�<&��<E��<��<�DZ<� /<�R<�:<���<��W<��3<��E<�T�<O�<#�
<#�
<)�o<#�
<:^<UF<(O)<:^<#�
<#�
<.��<,2< h<#�
<#�
<#�
<#�
<#�
<#�
<#�
<G|�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<77<#�
<#�
<#�
<#�
<#�
<#�
<#�
<@^�<h;<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018022202151020180222021510IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018022322462820180223224628QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018022322462820180223224628QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550520190521075505IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                